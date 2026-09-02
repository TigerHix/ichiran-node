use std::cell::RefCell;
use std::io::Read;

use flate2::bufread::GzDecoder;
use serde::Serialize;

use crate::binary::{
    align, assert_zero, checked_range, checked_table_end, crc32, magic, u16_at, u32_at,
};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;

const MAGIC: &[u8; 8] = b"ICHIDETL";
const VERSION: u16 = 2;
const HEADER_BYTES: usize = 96;
const ENTRY_BYTES: usize = 8;
const BLOCK_BYTES: usize = 24;
const PROPERTY_TAGS: [&str; 7] = ["dial", "field", "misc", "pos", "s_inf", "stagk", "stagr"];

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DetailRange {
    pub block: u32,
    pub offset: u32,
    pub byte_length: u32,
    pub uncompressed_bytes: u32,
    pub checksum: u32,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DetailGloss {
    pub ord: u32,
    pub text: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DetailProperty {
    pub tag: &'static str,
    pub ord: u32,
    pub text: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DetailSense {
    pub ord: u32,
    pub glosses: Vec<DetailGloss>,
    pub properties: Vec<DetailProperty>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DetailForm {
    pub route: Route,
    pub ord: u32,
    pub common: Option<u32>,
    pub text: String,
    pub common_tags: String,
    pub conjugatable: bool,
    pub nokanji: bool,
    pub best: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DetailEntry {
    pub seq: u32,
    pub forms: Vec<DetailForm>,
    pub senses: Vec<DetailSense>,
}

pub struct DetailStore {
    total_bytes: usize,
    entry_count: usize,
    block_count: usize,
    entries_offset: usize,
    blocks_offset: usize,
    data_offset: usize,
    prefix: Vec<u8>,
    cache: RefCell<Option<(usize, Vec<u8>)>>,
}

impl DetailStore {
    pub fn prefix_length(header: &[u8], total_bytes: usize) -> Result<usize> {
        let parsed = Header::read(header, total_bytes)?;
        Ok(parsed.data_offset)
    }

    pub fn open(prefix: Vec<u8>, total_bytes: usize) -> Result<Self> {
        let header = Header::read(&prefix, total_bytes)?;
        if prefix.len() != header.data_offset {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                format!(
                    "detail prefix has {} bytes; expected {}",
                    prefix.len(),
                    header.data_offset
                ),
            ));
        }
        let entries_end = checked_table_end(
            header.entries_offset,
            header.entry_count,
            ENTRY_BYTES,
            prefix.len(),
            ErrorCode::CorruptIndex,
            "detail entry index",
        )?;
        let index = checked_range(
            &prefix,
            header.entries_offset,
            entries_end - header.entries_offset,
            ErrorCode::CorruptIndex,
            "detail entry index",
        )?;
        let blocks_end = checked_table_end(
            header.blocks_offset,
            header.block_count,
            BLOCK_BYTES,
            prefix.len(),
            ErrorCode::CorruptIndex,
            "detail block table",
        )?;
        let blocks = checked_range(
            &prefix,
            header.blocks_offset,
            blocks_end - header.blocks_offset,
            ErrorCode::CorruptIndex,
            "detail block table",
        )?;
        if crc32(index) != header.entries_checksum || crc32(blocks) != header.blocks_checksum {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "detail index checksum does not match",
            ));
        }
        assert_zero(
            &prefix,
            entries_end,
            header.blocks_offset,
            ErrorCode::CorruptIndex,
            "detail index padding",
        )?;
        assert_zero(
            &prefix,
            blocks_end,
            header.data_offset,
            ErrorCode::CorruptIndex,
            "detail block-table padding",
        )?;
        let mut next_entry = 0;
        let mut next_data = 0;
        for block in 0..header.block_count {
            let at = checked_table_end(
                header.blocks_offset,
                block,
                BLOCK_BYTES,
                prefix.len(),
                ErrorCode::CorruptIndex,
                "detail block index",
            )?;
            let data = u32_at(
                &prefix,
                at,
                ErrorCode::CorruptIndex,
                "detail block data offset",
            )? as usize;
            let compressed = u32_at(
                &prefix,
                at + 4,
                ErrorCode::CorruptIndex,
                "detail block compressed length",
            )? as usize;
            let uncompressed = u32_at(
                &prefix,
                at + 8,
                ErrorCode::CorruptIndex,
                "detail block uncompressed length",
            )? as usize;
            let first_entry = u32_at(
                &prefix,
                at + 16,
                ErrorCode::CorruptIndex,
                "detail block first entry",
            )? as usize;
            let count = u32_at(
                &prefix,
                at + 20,
                ErrorCode::CorruptIndex,
                "detail block entry count",
            )? as usize;
            let entry_end = first_entry.checked_add(count).ok_or_else(|| {
                KernelError::new(ErrorCode::CorruptIndex, "detail entry range overflows")
            })?;
            let relative_data_end = data.checked_add(compressed).ok_or_else(|| {
                KernelError::new(ErrorCode::CorruptIndex, "detail block data range overflows")
            })?;
            let absolute_data_end = header
                .data_offset
                .checked_add(relative_data_end)
                .ok_or_else(|| {
                    KernelError::new(ErrorCode::CorruptIndex, "detail block range overflows")
                })?;
            if data != next_data
                || compressed == 0
                || uncompressed == 0
                || first_entry != next_entry
                || count == 0
                || entry_end > header.entry_count
                || absolute_data_end > total_bytes
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptIndex,
                    format!("detail block {block} is not canonical"),
                ));
            }
            let mut previous_record = None;
            for entry in first_entry..entry_end {
                let entry_at = checked_table_end(
                    header.entries_offset,
                    entry,
                    ENTRY_BYTES,
                    prefix.len(),
                    ErrorCode::CorruptIndex,
                    "detail entry index",
                )?;
                let entry_block = u32_at(
                    &prefix,
                    entry_at,
                    ErrorCode::CorruptIndex,
                    "detail entry block",
                )? as usize;
                let record = u32_at(
                    &prefix,
                    entry_at + 4,
                    ErrorCode::CorruptIndex,
                    "detail record offset",
                )? as usize;
                let record_end = record.checked_add(4).ok_or_else(|| {
                    KernelError::new(ErrorCode::CorruptIndex, "detail record offset overflows")
                })?;
                if entry_block != block
                    || previous_record.is_some_and(|prior| record <= prior)
                    || record_end > uncompressed
                {
                    return Err(KernelError::new(
                        ErrorCode::CorruptIndex,
                        format!("detail entry {entry} is not canonical"),
                    ));
                }
                previous_record = Some(record);
            }
            next_entry = entry_end;
            next_data = relative_data_end;
        }
        let covered_bytes = header.data_offset.checked_add(next_data).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptIndex, "detail store length overflows")
        })?;
        if next_entry != header.entry_count || covered_bytes != total_bytes {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "detail blocks do not cover the store",
            ));
        }
        Ok(Self {
            total_bytes,
            entry_count: header.entry_count,
            block_count: header.block_count,
            entries_offset: header.entries_offset,
            blocks_offset: header.blocks_offset,
            data_offset: header.data_offset,
            prefix,
            cache: RefCell::new(None),
        })
    }

    pub fn resident_bytes(&self) -> usize {
        self.prefix.len()
            + self
                .cache
                .borrow()
                .as_ref()
                .map_or(0, |(_, bytes)| bytes.len())
    }

    pub fn range(&self, entry_index: u32) -> Result<DetailRange> {
        let entry = self.entry_index(entry_index)?;
        let at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(
            &self.prefix,
            at,
            ErrorCode::CorruptIndex,
            "detail entry block",
        )? as usize;
        self.block_range(block)
    }

    pub fn entry_from_compressed(
        &self,
        entry_index: u32,
        compressed: &[u8],
    ) -> Result<DetailEntry> {
        let entry = self.entry_index(entry_index)?;
        let entry_at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(
            &self.prefix,
            entry_at,
            ErrorCode::CorruptIndex,
            "detail entry block",
        )? as usize;
        let range = self.block_range(block)?;
        if compressed.len() != range.byte_length as usize {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                "supplied detail range has the wrong byte length",
            ));
        }
        let decoded = gunzip(compressed, range.uncompressed_bytes as usize)?;
        if crc32(&decoded) != range.checksum {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "detail block checksum does not match",
            ));
        }
        *self.cache.borrow_mut() = Some((block, decoded));
        self.entry_from_cache(entry)
    }

    pub fn entry_cached(&self, entry_index: u32) -> Result<Option<DetailEntry>> {
        let entry = self.entry_index(entry_index)?;
        let entry_at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(
            &self.prefix,
            entry_at,
            ErrorCode::CorruptIndex,
            "detail entry block",
        )? as usize;
        if self
            .cache
            .borrow()
            .as_ref()
            .is_some_and(|(cached, _)| *cached == block)
        {
            self.entry_from_cache(entry).map(Some)
        } else {
            Ok(None)
        }
    }

    fn entry_from_cache(&self, entry: usize) -> Result<DetailEntry> {
        let at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(
            &self.prefix,
            at,
            ErrorCode::CorruptIndex,
            "detail entry block",
        )? as usize;
        let record_offset = u32_at(
            &self.prefix,
            at + 4,
            ErrorCode::CorruptIndex,
            "detail record offset",
        )? as usize;
        let cache = self.cache.borrow();
        let (_, bytes) = cache
            .as_ref()
            .filter(|(cached, _)| *cached == block)
            .ok_or_else(|| {
                KernelError::new(ErrorCode::InvalidInput, "detail block is not cached")
            })?;
        let record_bytes = u32_at(
            bytes,
            record_offset,
            ErrorCode::CorruptBlock,
            "detail record length",
        )? as usize;
        let record_start = record_offset.checked_add(4).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptBlock, "detail record offset overflows")
        })?;
        let record = checked_range(
            bytes,
            record_start,
            record_bytes,
            ErrorCode::CorruptBlock,
            "detail record",
        )?;
        decode_entry(record)
    }

    fn block_range(&self, block: usize) -> Result<DetailRange> {
        if block >= self.block_count {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "detail block is out of range",
            ));
        }
        let at = self.blocks_offset + block * BLOCK_BYTES;
        let relative = u32_at(
            &self.prefix,
            at,
            ErrorCode::CorruptIndex,
            "detail block data offset",
        )?;
        let byte_length = u32_at(
            &self.prefix,
            at + 4,
            ErrorCode::CorruptIndex,
            "detail block compressed length",
        )?;
        let uncompressed_bytes = u32_at(
            &self.prefix,
            at + 8,
            ErrorCode::CorruptIndex,
            "detail block decoded length",
        )?;
        let checksum = u32_at(
            &self.prefix,
            at + 12,
            ErrorCode::CorruptIndex,
            "detail block checksum",
        )?;
        let offset = self
            .data_offset
            .checked_add(relative as usize)
            .ok_or_else(|| {
                KernelError::new(ErrorCode::CorruptIndex, "detail block offset overflows")
            })?;
        let end = offset.checked_add(byte_length as usize).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptIndex, "detail block range overflows")
        })?;
        if end > self.total_bytes {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "detail block lies outside the store",
            ));
        }
        let offset = u32::try_from(offset).map_err(|_| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "detail block offset exceeds the format limit",
            )
        })?;
        Ok(DetailRange {
            block: block as u32,
            offset,
            byte_length,
            uncompressed_bytes,
            checksum,
        })
    }

    fn entry_index(&self, entry: u32) -> Result<usize> {
        if entry as usize >= self.entry_count {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                format!("detail entry {entry} is out of range"),
            ));
        }
        Ok(entry as usize)
    }
}

struct Header {
    entry_count: usize,
    block_count: usize,
    entries_offset: usize,
    blocks_offset: usize,
    data_offset: usize,
    entries_checksum: u32,
    blocks_checksum: u32,
}

impl Header {
    fn read(bytes: &[u8], total_bytes: usize) -> Result<Self> {
        if bytes.len() < HEADER_BYTES || !magic(bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete ICHIDETL header",
            ));
        }
        let version = u16_at(bytes, 8, ErrorCode::InvalidHeader, "detail version")?;
        if version != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported detail version {version}"),
            ));
        }
        if u16_at(bytes, 10, ErrorCode::InvalidHeader, "detail header size")? as usize
            != HEADER_BYTES
            || u32_at(bytes, 12, ErrorCode::InvalidHeader, "detail flags")? != 0
            || u16_at(bytes, 32, ErrorCode::InvalidHeader, "detail entry stride")? as usize
                != ENTRY_BYTES
            || u16_at(bytes, 34, ErrorCode::InvalidHeader, "detail block stride")? as usize
                != BLOCK_BYTES
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "detail header sizes or flags are invalid",
            ));
        }
        assert_zero(
            bytes,
            60,
            HEADER_BYTES,
            ErrorCode::InvalidHeader,
            "detail reserved header",
        )?;
        let mut header = bytes[..HEADER_BYTES].to_vec();
        header[20..24].fill(0);
        if crc32(&header)
            != u32_at(
                bytes,
                20,
                ErrorCode::InvalidHeader,
                "detail header checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "detail header checksum does not match",
            ));
        }
        let entry_count =
            u32_at(bytes, 24, ErrorCode::InvalidHeader, "detail entry count")? as usize;
        let block_count =
            u32_at(bytes, 28, ErrorCode::InvalidHeader, "detail block count")? as usize;
        let target_block_bytes = u32_at(
            bytes,
            36,
            ErrorCode::InvalidHeader,
            "detail target block bytes",
        )?;
        let entries_offset =
            u32_at(bytes, 40, ErrorCode::InvalidHeader, "detail entries offset")? as usize;
        let blocks_offset =
            u32_at(bytes, 44, ErrorCode::InvalidHeader, "detail blocks offset")? as usize;
        let data_offset =
            u32_at(bytes, 48, ErrorCode::InvalidHeader, "detail data offset")? as usize;
        let entries_end = checked_table_end(
            entries_offset,
            entry_count,
            ENTRY_BYTES,
            total_bytes,
            ErrorCode::InvalidHeader,
            "detail entry index",
        )?;
        let blocks_end = checked_table_end(
            blocks_offset,
            block_count,
            BLOCK_BYTES,
            total_bytes,
            ErrorCode::InvalidHeader,
            "detail block index",
        )?;
        if u32_at(bytes, 16, ErrorCode::InvalidHeader, "detail total size")? as usize != total_bytes
            || entry_count == 0
            || block_count == 0
            || target_block_bytes == 0
            || entries_offset != HEADER_BYTES
            || blocks_offset != align(entries_end, 8)?
            || data_offset != align(blocks_end, 8)?
            || data_offset > total_bytes
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "detail offsets or counts are invalid",
            ));
        }
        Ok(Self {
            entry_count,
            block_count,
            entries_offset,
            blocks_offset,
            data_offset,
            entries_checksum: u32_at(bytes, 52, ErrorCode::InvalidHeader, "detail entry checksum")?,
            blocks_checksum: u32_at(bytes, 56, ErrorCode::InvalidHeader, "detail block checksum")?,
        })
    }
}

struct Cursor<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Cursor<'a> {
    fn uint(&mut self) -> Result<u32> {
        let mut value = 0_u64;
        let mut shift = 0;
        for _ in 0..5 {
            let byte = self.byte()?;
            value += u64::from(byte & 0x7f) << shift;
            if byte & 0x80 == 0 {
                return u32::try_from(value).map_err(|_| {
                    KernelError::new(ErrorCode::CorruptBlock, "detail varint exceeds uint32")
                });
            }
            shift += 7;
        }
        Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "detail varint is not canonical uint32",
        ))
    }

    fn byte(&mut self) -> Result<u8> {
        let value = self.bytes.get(self.offset).copied().ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptBlock, "detail record is truncated")
        })?;
        self.offset += 1;
        Ok(value)
    }

    fn text(&mut self) -> Result<String> {
        let length = self.uint()? as usize;
        let value = checked_range(
            self.bytes,
            self.offset,
            length,
            ErrorCode::CorruptBlock,
            "detail string",
        )?;
        self.offset += length;
        std::str::from_utf8(value).map(str::to_owned).map_err(|_| {
            KernelError::new(ErrorCode::CorruptBlock, "detail string is not valid UTF-8")
        })
    }

    fn count(&mut self, label: &str) -> Result<usize> {
        let count = self.uint()? as usize;
        if count > self.bytes.len() - self.offset {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                format!("{label} exceeds the remaining record bytes"),
            ));
        }
        Ok(count)
    }
}

fn decode_entry(bytes: &[u8]) -> Result<DetailEntry> {
    let mut cursor = Cursor { bytes, offset: 0 };
    let seq = cursor.uint()?;
    let form_count = cursor.count("detail form count")?;
    let mut forms = Vec::with_capacity(form_count);
    for _ in 0..form_count {
        let flags = cursor.byte()?;
        if flags & 0xf0 != 0 {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "detail form has unknown flags",
            ));
        }
        let ord = cursor.uint()?;
        let common = cursor.uint()?;
        let text = cursor.text()?;
        let common_tags = cursor.text()?;
        let best = if flags & (1 << 3) != 0 {
            Some(cursor.text()?)
        } else {
            None
        };
        forms.push(DetailForm {
            route: if flags & 1 != 0 {
                Route::Kana
            } else {
                Route::Kanji
            },
            text,
            ord,
            common: common.checked_sub(1),
            common_tags,
            conjugatable: flags & (1 << 1) != 0,
            nokanji: flags & (1 << 2) != 0,
            best,
        });
    }
    let sense_count = cursor.count("detail sense count")?;
    let mut senses = Vec::with_capacity(sense_count);
    for _ in 0..sense_count {
        let ord = cursor.uint()?;
        let gloss_count = cursor.count("detail gloss count")?;
        let mut glosses = Vec::with_capacity(gloss_count);
        for _ in 0..gloss_count {
            glosses.push(DetailGloss {
                ord: cursor.uint()?,
                text: cursor.text()?,
            });
        }
        let property_count = cursor.count("detail property count")?;
        let mut properties = Vec::with_capacity(property_count);
        for _ in 0..property_count {
            let tag = PROPERTY_TAGS
                .get(cursor.byte()? as usize)
                .copied()
                .ok_or_else(|| {
                    KernelError::new(
                        ErrorCode::CorruptBlock,
                        "detail property has an unknown tag",
                    )
                })?;
            properties.push(DetailProperty {
                tag,
                ord: cursor.uint()?,
                text: cursor.text()?,
            });
        }
        senses.push(DetailSense {
            ord,
            glosses,
            properties,
        });
    }
    if cursor.offset != bytes.len() {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "detail record has trailing bytes",
        ));
    }
    Ok(DetailEntry { seq, forms, senses })
}

fn gunzip(compressed: &[u8], expected: usize) -> Result<Vec<u8>> {
    let limit = expected.checked_add(1).ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            "detail gzip decoded length overflows",
        )
    })?;
    let limit = u64::try_from(limit).map_err(|_| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            "detail gzip decoded length exceeds the reader limit",
        )
    })?;
    let decoder = GzDecoder::new(compressed);
    let mut bounded = decoder.take(limit);
    let mut decoded = Vec::new();
    bounded.read_to_end(&mut decoded).map_err(|error| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            format!("detail gzip decode failed: {error}"),
        )
    })?;
    if decoded.len() != expected {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!(
                "detail gzip decoded {} bytes; expected {expected}",
                decoded.len()
            ),
        ));
    }
    let decoder = bounded.into_inner();
    if !decoder.get_ref().is_empty() {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "detail gzip range has trailing compressed bytes",
        ));
    }
    Ok(decoded)
}

#[cfg(test)]
mod tests {
    use std::io::Write;

    use flate2::{Compression, write::GzEncoder};

    use super::*;

    fn gzip(bytes: &[u8]) -> Vec<u8> {
        let mut encoder = GzEncoder::new(Vec::new(), Compression::fast());
        encoder.write_all(bytes).unwrap();
        encoder.finish().unwrap()
    }

    fn put_u16(bytes: &mut [u8], offset: usize, value: u16) {
        bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
    }

    fn put_u32(bytes: &mut [u8], offset: usize, value: u32) {
        bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
    }

    #[test]
    fn detail_header_rejects_count_that_overflows_wasm32_table_math() {
        let mut bytes = vec![0; HEADER_BYTES];
        bytes[..MAGIC.len()].copy_from_slice(MAGIC);
        put_u16(&mut bytes, 8, VERSION);
        put_u16(&mut bytes, 10, HEADER_BYTES as u16);
        put_u32(&mut bytes, 16, HEADER_BYTES as u32);
        put_u32(&mut bytes, 24, u32::MAX);
        put_u32(&mut bytes, 28, 1);
        put_u16(&mut bytes, 32, ENTRY_BYTES as u16);
        put_u16(&mut bytes, 34, BLOCK_BYTES as u16);
        put_u32(&mut bytes, 36, 1);
        for offset in [40, 44, 48] {
            put_u32(&mut bytes, offset, HEADER_BYTES as u32);
        }
        let checksum = crc32(&bytes);
        put_u32(&mut bytes, 20, checksum);

        let error = Header::read(&bytes, bytes.len())
            .err()
            .expect("oversized detail entry table was accepted");
        assert_eq!(error.code, ErrorCode::InvalidHeader);
        assert!(error.message.contains("detail entry index"));
    }

    #[test]
    fn detail_record_rejects_impossible_item_count_before_allocation() {
        let error = decode_entry(&[0, 0xff, 0xff, 0xff, 0xff, 0x0f]).unwrap_err();

        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(
            error.message,
            "detail form count exceeds the remaining record bytes"
        );
    }

    #[test]
    fn detail_gzip_rejects_oversized_expansion_at_expected_plus_one() {
        let compressed = gzip(&vec![0; 1024 * 1024]);
        let error = gunzip(&compressed, 16).unwrap_err();

        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(error.message, "detail gzip decoded 17 bytes; expected 16");
    }

    #[test]
    fn detail_gzip_rejects_trailing_compressed_bytes() {
        let mut compressed = gzip(b"exact");
        compressed.push(0);
        let error = gunzip(&compressed, 5).unwrap_err();
        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert!(error.message.contains("trailing compressed bytes"));
    }

    #[test]
    fn detail_gzip_rejects_expected_length_overflow() {
        let error = gunzip(&[], usize::MAX).unwrap_err();

        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(error.message, "detail gzip decoded length overflows");
    }
}
