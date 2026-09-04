use std::cell::RefCell;
use std::collections::HashSet;
use std::io::Read;

use flate2::bufread::GzDecoder;
use serde::Serialize;

use crate::binary::{
    align, assert_zero, checked_range, checked_table_end, crc32, magic, u16_at, u32_at,
};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;

const LEXICON_MAGIC: &[u8; 8] = b"ICHILEXI";
const GLOSS_MAGIC: &[u8; 8] = b"ICHIGLOS";
const FORMAT_VERSION: u16 = 1;
const LEXICON_HEADER_BYTES: usize = 96;
const GLOSS_HEADER_BYTES: usize = 128;
const ENTRY_BYTES: usize = 8;
const BLOCK_BYTES: usize = 24;
const LEXICON_PROPERTY_TAGS: [&str; 6] = ["dial", "field", "misc", "pos", "stagk", "stagr"];

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DictionaryRange {
    pub block: u32,
    pub offset: u32,
    pub byte_length: u32,
    pub uncompressed_bytes: u32,
    pub checksum: u32,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DictionaryGloss {
    pub ord: u32,
    pub text: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DictionaryProperty {
    pub tag: &'static str,
    pub ord: u32,
    pub text: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct DictionarySense {
    pub ord: u32,
    pub glosses: Vec<DictionaryGloss>,
    pub properties: Vec<DictionaryProperty>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DictionaryForm {
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
pub struct DictionaryEntry {
    pub seq: u32,
    pub forms: Vec<DictionaryForm>,
    pub senses: Vec<DictionarySense>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct LexiconSense {
    pub ord: u32,
    pub properties: Vec<DictionaryProperty>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct LexiconEntry {
    pub seq: u32,
    pub forms: Vec<DictionaryForm>,
    pub senses: Vec<LexiconSense>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct LocaleGroup {
    pub targets: Vec<u32>,
    pub glosses: Vec<DictionaryGloss>,
    pub info: Vec<DictionaryGloss>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct LocaleEntry {
    pub seq: u32,
    pub groups: Vec<LocaleGroup>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum DictionaryStoreKind {
    Lexicon,
    Locale,
    Fallback,
}

pub struct DictionaryStores<'a> {
    pub lexicon: &'a LexiconStore,
    pub locale: &'a LocaleStore,
    pub fallback: &'a LocaleStore,
}

struct IndexedStore {
    total_bytes: usize,
    entry_count: usize,
    block_count: usize,
    entries_offset: usize,
    blocks_offset: usize,
    data_offset: usize,
    prefix: Vec<u8>,
    cache: RefCell<Option<(usize, Vec<u8>)>>,
    label: &'static str,
}

struct StoreHeader {
    entry_count: usize,
    block_count: usize,
    entries_offset: usize,
    blocks_offset: usize,
    data_offset: usize,
    entries_checksum: u32,
    blocks_checksum: u32,
}

impl IndexedStore {
    fn prefix_length(
        header: &[u8],
        total_bytes: usize,
        expected_magic: &[u8; 8],
        header_bytes: usize,
        label: &'static str,
    ) -> Result<usize> {
        Ok(
            StoreHeader::read(header, total_bytes, expected_magic, header_bytes, label)?
                .data_offset,
        )
    }

    fn open(
        prefix: Vec<u8>,
        total_bytes: usize,
        expected_magic: &[u8; 8],
        header_bytes: usize,
        label: &'static str,
    ) -> Result<Self> {
        let header = StoreHeader::read(&prefix, total_bytes, expected_magic, header_bytes, label)?;
        if prefix.len() != header.data_offset {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                format!(
                    "{label} prefix has {} bytes; expected {}",
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
            &format!("{label} entry index"),
        )?;
        let entries = checked_range(
            &prefix,
            header.entries_offset,
            entries_end - header.entries_offset,
            ErrorCode::CorruptIndex,
            &format!("{label} entry index"),
        )?;
        let blocks_end = checked_table_end(
            header.blocks_offset,
            header.block_count,
            BLOCK_BYTES,
            prefix.len(),
            ErrorCode::CorruptIndex,
            &format!("{label} block table"),
        )?;
        let blocks = checked_range(
            &prefix,
            header.blocks_offset,
            blocks_end - header.blocks_offset,
            ErrorCode::CorruptIndex,
            &format!("{label} block table"),
        )?;
        if crc32(entries) != header.entries_checksum || crc32(blocks) != header.blocks_checksum {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                format!("{label} index checksum does not match"),
            ));
        }
        assert_zero(
            &prefix,
            entries_end,
            header.blocks_offset,
            ErrorCode::CorruptIndex,
            &format!("{label} entry-index padding"),
        )?;
        assert_zero(
            &prefix,
            blocks_end,
            header.data_offset,
            ErrorCode::CorruptIndex,
            &format!("{label} block-table padding"),
        )?;

        let mut next_entry = 0;
        let mut next_data = 0;
        for block in 0..header.block_count {
            let at = header.blocks_offset + block * BLOCK_BYTES;
            let data = u32_at(&prefix, at, ErrorCode::CorruptIndex, "block data offset")? as usize;
            let compressed = u32_at(
                &prefix,
                at + 4,
                ErrorCode::CorruptIndex,
                "block compressed length",
            )? as usize;
            let uncompressed = u32_at(
                &prefix,
                at + 8,
                ErrorCode::CorruptIndex,
                "block uncompressed length",
            )? as usize;
            let first_entry = u32_at(
                &prefix,
                at + 16,
                ErrorCode::CorruptIndex,
                "block first entry",
            )? as usize;
            let count = u32_at(
                &prefix,
                at + 20,
                ErrorCode::CorruptIndex,
                "block entry count",
            )? as usize;
            let entry_end = first_entry.checked_add(count).ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    format!("{label} entry range overflows"),
                )
            })?;
            let relative_data_end = data.checked_add(compressed).ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    format!("{label} data range overflows"),
                )
            })?;
            let absolute_data_end = header
                .data_offset
                .checked_add(relative_data_end)
                .ok_or_else(|| {
                    KernelError::new(
                        ErrorCode::CorruptIndex,
                        format!("{label} block range overflows"),
                    )
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
                    format!("{label} block {block} is not canonical"),
                ));
            }
            let mut previous_record = None;
            for entry in first_entry..entry_end {
                let entry_at = header.entries_offset + entry * ENTRY_BYTES;
                let entry_block =
                    u32_at(&prefix, entry_at, ErrorCode::CorruptIndex, "entry block")? as usize;
                let record = u32_at(
                    &prefix,
                    entry_at + 4,
                    ErrorCode::CorruptIndex,
                    "entry record offset",
                )? as usize;
                if entry_block != block
                    || previous_record.is_some_and(|prior| record <= prior)
                    || record.checked_add(4).is_none_or(|end| end > uncompressed)
                {
                    return Err(KernelError::new(
                        ErrorCode::CorruptIndex,
                        format!("{label} entry {entry} is not canonical"),
                    ));
                }
                previous_record = Some(record);
            }
            next_entry = entry_end;
            next_data = relative_data_end;
        }
        if next_entry != header.entry_count
            || header.data_offset.checked_add(next_data) != Some(total_bytes)
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                format!("{label} blocks do not cover the store"),
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
            label,
        })
    }

    fn resident_bytes(&self) -> usize {
        self.prefix.len()
            + self
                .cache
                .borrow()
                .as_ref()
                .map_or(0, |(_, bytes)| bytes.len())
    }

    fn entry_count(&self) -> usize {
        self.entry_count
    }

    fn range(&self, entry_index: u32) -> Result<DictionaryRange> {
        let entry = self.entry_index(entry_index)?;
        let at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(&self.prefix, at, ErrorCode::CorruptIndex, "entry block")? as usize;
        self.block_range(block)
    }

    fn record_from_compressed(&self, entry_index: u32, compressed: &[u8]) -> Result<Vec<u8>> {
        let entry = self.entry_index(entry_index)?;
        let entry_at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(
            &self.prefix,
            entry_at,
            ErrorCode::CorruptIndex,
            "entry block",
        )? as usize;
        let range = self.block_range(block)?;
        if compressed.len() != range.byte_length as usize {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                format!("supplied {} range has the wrong byte length", self.label),
            ));
        }
        let decoded = gunzip(compressed, range.uncompressed_bytes as usize, self.label)?;
        if crc32(&decoded) != range.checksum {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                format!("{} block checksum does not match", self.label),
            ));
        }
        *self.cache.borrow_mut() = Some((block, decoded));
        self.record_from_cache(entry)
    }

    fn record_cached(&self, entry_index: u32) -> Result<Option<Vec<u8>>> {
        let entry = self.entry_index(entry_index)?;
        let entry_at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(
            &self.prefix,
            entry_at,
            ErrorCode::CorruptIndex,
            "entry block",
        )? as usize;
        if self
            .cache
            .borrow()
            .as_ref()
            .is_some_and(|(cached, _)| *cached == block)
        {
            self.record_from_cache(entry).map(Some)
        } else {
            Ok(None)
        }
    }

    fn record_from_cache(&self, entry: usize) -> Result<Vec<u8>> {
        let at = self.entries_offset + entry * ENTRY_BYTES;
        let block = u32_at(&self.prefix, at, ErrorCode::CorruptIndex, "entry block")? as usize;
        let record_offset = u32_at(
            &self.prefix,
            at + 4,
            ErrorCode::CorruptIndex,
            "entry record offset",
        )? as usize;
        let cache = self.cache.borrow();
        let (_, bytes) = cache
            .as_ref()
            .filter(|(cached, _)| *cached == block)
            .ok_or_else(|| {
                KernelError::new(
                    ErrorCode::InvalidInput,
                    format!("{} block is not cached", self.label),
                )
            })?;
        let record_bytes = u32_at(
            bytes,
            record_offset,
            ErrorCode::CorruptBlock,
            "entry record length",
        )? as usize;
        let record_start = record_offset.checked_add(4).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptBlock, "entry record offset overflows")
        })?;
        Ok(checked_range(
            bytes,
            record_start,
            record_bytes,
            ErrorCode::CorruptBlock,
            "entry record",
        )?
        .to_vec())
    }

    fn block_range(&self, block: usize) -> Result<DictionaryRange> {
        if block >= self.block_count {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                format!("{} block is out of range", self.label),
            ));
        }
        let at = self.blocks_offset + block * BLOCK_BYTES;
        let relative = u32_at(
            &self.prefix,
            at,
            ErrorCode::CorruptIndex,
            "block data offset",
        )?;
        let byte_length = u32_at(
            &self.prefix,
            at + 4,
            ErrorCode::CorruptIndex,
            "block compressed length",
        )?;
        let uncompressed_bytes = u32_at(
            &self.prefix,
            at + 8,
            ErrorCode::CorruptIndex,
            "block decoded length",
        )?;
        let checksum = u32_at(
            &self.prefix,
            at + 12,
            ErrorCode::CorruptIndex,
            "block checksum",
        )?;
        let offset = self
            .data_offset
            .checked_add(relative as usize)
            .ok_or_else(|| KernelError::new(ErrorCode::CorruptIndex, "block offset overflows"))?;
        if offset
            .checked_add(byte_length as usize)
            .is_none_or(|end| end > self.total_bytes)
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "block lies outside the store",
            ));
        }
        Ok(DictionaryRange {
            block: block as u32,
            offset: u32::try_from(offset).map_err(|_| {
                KernelError::new(ErrorCode::CorruptIndex, "block offset exceeds format limit")
            })?,
            byte_length,
            uncompressed_bytes,
            checksum,
        })
    }

    fn entry_index(&self, entry: u32) -> Result<usize> {
        if entry as usize >= self.entry_count {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                format!("{} entry {entry} is out of range", self.label),
            ));
        }
        Ok(entry as usize)
    }
}

impl StoreHeader {
    fn read(
        bytes: &[u8],
        total_bytes: usize,
        expected_magic: &[u8; 8],
        header_bytes: usize,
        label: &str,
    ) -> Result<Self> {
        if bytes.len() < header_bytes || !magic(bytes, expected_magic) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                format!("expected a complete {label} header"),
            ));
        }
        let version = u16_at(bytes, 8, ErrorCode::InvalidHeader, "dictionary version")?;
        if version != FORMAT_VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported {label} version {version}"),
            ));
        }
        if u16_at(
            bytes,
            10,
            ErrorCode::InvalidHeader,
            "dictionary header size",
        )? as usize
            != header_bytes
            || u32_at(bytes, 12, ErrorCode::InvalidHeader, "dictionary flags")? != 0
            || u16_at(
                bytes,
                32,
                ErrorCode::InvalidHeader,
                "dictionary entry stride",
            )? as usize
                != ENTRY_BYTES
            || u16_at(
                bytes,
                34,
                ErrorCode::InvalidHeader,
                "dictionary block stride",
            )? as usize
                != BLOCK_BYTES
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                format!("{label} header sizes or flags are invalid"),
            ));
        }
        let mut header = bytes[..header_bytes].to_vec();
        header[20..24].fill(0);
        if crc32(&header)
            != u32_at(
                bytes,
                20,
                ErrorCode::InvalidHeader,
                "dictionary header checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                format!("{label} header checksum does not match"),
            ));
        }
        let entry_count = u32_at(
            bytes,
            24,
            ErrorCode::InvalidHeader,
            "dictionary entry count",
        )? as usize;
        let block_count = u32_at(
            bytes,
            28,
            ErrorCode::InvalidHeader,
            "dictionary block count",
        )? as usize;
        let target_block_bytes = u32_at(
            bytes,
            36,
            ErrorCode::InvalidHeader,
            "dictionary target block bytes",
        )?;
        let entries_offset = u32_at(
            bytes,
            40,
            ErrorCode::InvalidHeader,
            "dictionary entries offset",
        )? as usize;
        let blocks_offset = u32_at(
            bytes,
            44,
            ErrorCode::InvalidHeader,
            "dictionary blocks offset",
        )? as usize;
        let data_offset = u32_at(
            bytes,
            48,
            ErrorCode::InvalidHeader,
            "dictionary data offset",
        )? as usize;
        let entries_end = checked_table_end(
            entries_offset,
            entry_count,
            ENTRY_BYTES,
            total_bytes,
            ErrorCode::InvalidHeader,
            "dictionary entry index",
        )?;
        let blocks_end = checked_table_end(
            blocks_offset,
            block_count,
            BLOCK_BYTES,
            total_bytes,
            ErrorCode::InvalidHeader,
            "dictionary block index",
        )?;
        if u32_at(bytes, 16, ErrorCode::InvalidHeader, "dictionary total size")? as usize
            != total_bytes
            || entry_count == 0
            || block_count == 0
            || target_block_bytes == 0
            || entries_offset != header_bytes
            || blocks_offset != align(entries_end, 8)?
            || data_offset != align(blocks_end, 8)?
            || data_offset > total_bytes
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                format!("{label} offsets or counts are invalid"),
            ));
        }
        Ok(Self {
            entry_count,
            block_count,
            entries_offset,
            blocks_offset,
            data_offset,
            entries_checksum: u32_at(
                bytes,
                52,
                ErrorCode::InvalidHeader,
                "dictionary entry checksum",
            )?,
            blocks_checksum: u32_at(
                bytes,
                56,
                ErrorCode::InvalidHeader,
                "dictionary block checksum",
            )?,
        })
    }
}

pub struct LexiconStore {
    inner: IndexedStore,
}

impl LexiconStore {
    pub fn prefix_length(header: &[u8], total_bytes: usize) -> Result<usize> {
        IndexedStore::prefix_length(
            header,
            total_bytes,
            LEXICON_MAGIC,
            LEXICON_HEADER_BYTES,
            "lexicon",
        )
    }

    pub fn open(prefix: Vec<u8>, total_bytes: usize) -> Result<Self> {
        assert_zero(
            &prefix,
            60,
            LEXICON_HEADER_BYTES,
            ErrorCode::InvalidHeader,
            "lexicon reserved header",
        )?;
        Ok(Self {
            inner: IndexedStore::open(
                prefix,
                total_bytes,
                LEXICON_MAGIC,
                LEXICON_HEADER_BYTES,
                "lexicon",
            )?,
        })
    }

    pub fn entry_count(&self) -> usize {
        self.inner.entry_count()
    }
    pub fn resident_bytes(&self) -> usize {
        self.inner.resident_bytes()
    }
    pub fn range(&self, entry_index: u32) -> Result<DictionaryRange> {
        self.inner.range(entry_index)
    }
    pub fn entry_from_compressed(
        &self,
        entry_index: u32,
        compressed: &[u8],
    ) -> Result<LexiconEntry> {
        decode_lexicon_entry(&self.inner.record_from_compressed(entry_index, compressed)?)
    }
    pub fn entry_cached(&self, entry_index: u32) -> Result<Option<LexiconEntry>> {
        self.inner
            .record_cached(entry_index)?
            .map(|record| decode_lexicon_entry(&record))
            .transpose()
    }
}

pub struct LocaleStore {
    inner: IndexedStore,
    locale: String,
    lexicon_sha256: [u8; 32],
}

impl LocaleStore {
    pub fn prefix_length(header: &[u8], total_bytes: usize) -> Result<usize> {
        IndexedStore::prefix_length(
            header,
            total_bytes,
            GLOSS_MAGIC,
            GLOSS_HEADER_BYTES,
            "locale gloss",
        )
    }

    pub fn open(
        prefix: Vec<u8>,
        total_bytes: usize,
        expected_lexicon_sha256: &[u8],
        expected_locale: &str,
        expected_entry_count: usize,
    ) -> Result<Self> {
        // Validate the complete fixed header before reading binding metadata.
        StoreHeader::read(
            &prefix,
            total_bytes,
            GLOSS_MAGIC,
            GLOSS_HEADER_BYTES,
            "locale gloss",
        )?;
        if expected_lexicon_sha256.len() != 32 {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                "expected lexicon SHA-256 must contain 32 bytes",
            ));
        }
        let mut digest = [0_u8; 32];
        digest.copy_from_slice(&prefix[60..92]);
        if digest != expected_lexicon_sha256 {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "locale gloss is bound to a different lexicon",
            ));
        }
        let locale_bytes = prefix[92] as usize;
        if locale_bytes == 0 || locale_bytes > 31 {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "locale gloss has an invalid locale length",
            ));
        }
        assert_zero(
            &prefix,
            93 + locale_bytes,
            128,
            ErrorCode::InvalidHeader,
            "locale gloss reserved header",
        )?;
        let locale = std::str::from_utf8(&prefix[93..93 + locale_bytes])
            .map_err(|_| {
                KernelError::new(
                    ErrorCode::InvalidHeader,
                    "locale gloss locale is not valid UTF-8",
                )
            })?
            .to_owned();
        if locale != expected_locale {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                format!("expected locale {expected_locale}; found {locale}"),
            ));
        }
        let inner = IndexedStore::open(
            prefix,
            total_bytes,
            GLOSS_MAGIC,
            GLOSS_HEADER_BYTES,
            "locale gloss",
        )?;
        if inner.entry_count() != expected_entry_count {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "locale gloss entry count does not match the lexicon",
            ));
        }
        Ok(Self {
            inner,
            locale,
            lexicon_sha256: digest,
        })
    }

    pub fn locale(&self) -> &str {
        &self.locale
    }
    pub fn lexicon_sha256(&self) -> &[u8; 32] {
        &self.lexicon_sha256
    }
    pub fn resident_bytes(&self) -> usize {
        self.inner.resident_bytes()
    }
    pub fn range(&self, entry_index: u32) -> Result<DictionaryRange> {
        self.inner.range(entry_index)
    }
    pub fn entry_from_compressed(
        &self,
        entry_index: u32,
        compressed: &[u8],
    ) -> Result<LocaleEntry> {
        decode_locale_entry(&self.inner.record_from_compressed(entry_index, compressed)?)
    }
    pub fn entry_cached(&self, entry_index: u32) -> Result<Option<LocaleEntry>> {
        self.inner
            .record_cached(entry_index)?
            .map(|record| decode_locale_entry(&record))
            .transpose()
    }
}

pub fn localize_entry(
    lexicon: &LexiconEntry,
    locale: &LocaleEntry,
    fallback: Option<&LocaleEntry>,
) -> Result<DictionaryEntry> {
    if locale.seq != lexicon.seq || fallback.is_some_and(|entry| entry.seq != lexicon.seq) {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "dictionary entry sequence does not match across stores",
        ));
    }
    let sense_ords = lexicon
        .senses
        .iter()
        .map(|sense| sense.ord)
        .collect::<HashSet<_>>();
    for entry in std::iter::once(locale).chain(fallback) {
        if entry
            .groups
            .iter()
            .flat_map(|group| &group.targets)
            .any(|target| !sense_ords.contains(target))
        {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "locale group targets a sense that is absent from the lexicon entry",
            ));
        }
    }
    let mut senses = Vec::with_capacity(lexicon.senses.len() + locale.groups.len());
    for sense in &lexicon.senses {
        let selected = exact_groups(locale, sense.ord);
        let selected_has_gloss = selected.iter().any(|group| !group.glosses.is_empty());
        let mut info = selected
            .iter()
            .flat_map(|group| group.info.iter())
            .collect::<Vec<_>>();
        let groups = if selected_has_gloss {
            selected
        } else {
            fallback.map_or_else(Vec::new, |entry| exact_groups(entry, sense.ord))
        };
        let mut properties = sense.properties.clone();
        if info.is_empty() {
            info = fallback.map_or_else(Vec::new, |entry| {
                exact_groups(entry, sense.ord)
                    .into_iter()
                    .flat_map(|group| group.info.iter())
                    .collect()
            });
        }
        properties.extend(info.into_iter().map(|value| DictionaryProperty {
            tag: "s_inf",
            ord: value.ord,
            text: value.text.clone(),
        }));
        senses.push(DictionarySense {
            ord: sense.ord,
            glosses: groups
                .into_iter()
                .flat_map(|group| group.glosses.iter().cloned())
                .collect(),
            properties,
        });
    }
    let selected_entry_wide = entry_groups(locale);
    let fallback_entry_wide = fallback.map_or_else(Vec::new, entry_groups);
    let gloss_groups = if selected_entry_wide
        .iter()
        .any(|group| !group.glosses.is_empty())
    {
        &selected_entry_wide
    } else {
        &fallback_entry_wide
    };
    let info_groups = if selected_entry_wide
        .iter()
        .any(|group| !group.info.is_empty())
    {
        &selected_entry_wide
    } else {
        &fallback_entry_wide
    };
    let glosses = gloss_groups
        .iter()
        .flat_map(|group| group.glosses.iter().cloned())
        .collect::<Vec<_>>();
    let info = info_groups
        .iter()
        .flat_map(|group| group.info.iter())
        .collect::<Vec<_>>();
    let next_ord = lexicon
        .senses
        .iter()
        .map(|sense| sense.ord)
        .max()
        .map_or(0, |ord| ord.saturating_add(1));
    if !glosses.is_empty() || !info.is_empty() {
        let mut properties = info
            .into_iter()
            .map(|value| DictionaryProperty {
                tag: "s_inf",
                ord: value.ord,
                text: value.text.clone(),
            })
            .collect::<Vec<_>>();
        properties.sort_by_key(|property| property.ord);
        senses.push(DictionarySense {
            ord: next_ord,
            glosses,
            properties,
        });
    }
    Ok(DictionaryEntry {
        seq: lexicon.seq,
        forms: lexicon.forms.clone(),
        senses,
    })
}

fn exact_groups(entry: &LocaleEntry, sense_ord: u32) -> Vec<&LocaleGroup> {
    entry
        .groups
        .iter()
        .filter(|group| group.targets.binary_search(&sense_ord).is_ok())
        .collect()
}

fn entry_groups(entry: &LocaleEntry) -> Vec<&LocaleGroup> {
    entry
        .groups
        .iter()
        .filter(|group| group.targets.is_empty())
        .collect()
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
                    KernelError::new(ErrorCode::CorruptBlock, "dictionary varint exceeds uint32")
                });
            }
            shift += 7;
        }
        Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "dictionary varint is not canonical uint32",
        ))
    }
    fn byte(&mut self) -> Result<u8> {
        let value = self.bytes.get(self.offset).copied().ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptBlock, "truncated dictionary byte")
        })?;
        self.offset += 1;
        Ok(value)
    }
    fn text(&mut self) -> Result<String> {
        let length = self.uint()? as usize;
        let end = self.offset.checked_add(length).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptBlock,
                "dictionary string length overflows",
            )
        })?;
        let bytes = self.bytes.get(self.offset..end).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptBlock, "truncated dictionary string")
        })?;
        self.offset = end;
        std::str::from_utf8(bytes).map(str::to_owned).map_err(|_| {
            KernelError::new(
                ErrorCode::CorruptBlock,
                "dictionary string is not valid UTF-8",
            )
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
    fn finish(&self) -> Result<()> {
        if self.offset != self.bytes.len() {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "dictionary record has trailing bytes",
            ));
        }
        Ok(())
    }
}

fn decode_lexicon_entry(bytes: &[u8]) -> Result<LexiconEntry> {
    let mut cursor = Cursor { bytes, offset: 0 };
    let seq = cursor.uint()?;
    let form_count = cursor.count("lexicon form count")?;
    let mut forms = Vec::with_capacity(form_count);
    for _ in 0..form_count {
        let flags = cursor.byte()?;
        if flags & 0xf0 != 0 {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "lexicon form has unknown flags",
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
        forms.push(DictionaryForm {
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
    let sense_count = cursor.count("lexicon sense count")?;
    let mut senses = Vec::with_capacity(sense_count);
    for _ in 0..sense_count {
        let ord = cursor.uint()?;
        let property_count = cursor.count("lexicon property count")?;
        let mut properties = Vec::with_capacity(property_count);
        for _ in 0..property_count {
            let tag = LEXICON_PROPERTY_TAGS
                .get(cursor.byte()? as usize)
                .copied()
                .ok_or_else(|| {
                    KernelError::new(
                        ErrorCode::CorruptBlock,
                        "lexicon property has an unknown tag",
                    )
                })?;
            properties.push(DictionaryProperty {
                tag,
                ord: cursor.uint()?,
                text: cursor.text()?,
            });
        }
        senses.push(LexiconSense { ord, properties });
    }
    cursor.finish()?;
    Ok(LexiconEntry { seq, forms, senses })
}

fn decode_locale_entry(bytes: &[u8]) -> Result<LocaleEntry> {
    let mut cursor = Cursor { bytes, offset: 0 };
    let seq = cursor.uint()?;
    let group_count = cursor.count("locale group count")?;
    let mut groups = Vec::with_capacity(group_count);
    let mut claimed_targets = HashSet::new();
    let mut previous_targets: Option<Vec<u32>> = None;
    for _ in 0..group_count {
        let target_count = cursor.count("locale target count")?;
        if target_count == 0 && group_count != 1 {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "locale entry mixes entry-wide and aligned groups",
            ));
        }
        let mut targets = Vec::with_capacity(target_count);
        for _ in 0..target_count {
            let target = cursor.uint()?;
            if targets.last().is_some_and(|previous| *previous >= target)
                || !claimed_targets.insert(target)
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptBlock,
                    "locale targets are duplicate or unordered",
                ));
            }
            targets.push(target);
        }
        if previous_targets
            .as_ref()
            .is_some_and(|previous| previous.as_slice() >= targets.as_slice())
        {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "locale groups are not ordered",
            ));
        }
        previous_targets = Some(targets.clone());
        let gloss_count = cursor.count("locale gloss count")?;
        let mut glosses = Vec::with_capacity(gloss_count);
        for _ in 0..gloss_count {
            let value = DictionaryGloss {
                ord: cursor.uint()?,
                text: cursor.text()?,
            };
            if value.text.is_empty()
                || glosses
                    .last()
                    .is_some_and(|prior: &DictionaryGloss| prior.ord >= value.ord)
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptBlock,
                    "locale glosses are empty or unordered",
                ));
            }
            glosses.push(value);
        }
        let info_count = cursor.count("locale info count")?;
        let mut info = Vec::with_capacity(info_count);
        for _ in 0..info_count {
            let value = DictionaryGloss {
                ord: cursor.uint()?,
                text: cursor.text()?,
            };
            if value.text.is_empty()
                || info
                    .last()
                    .is_some_and(|prior: &DictionaryGloss| prior.ord >= value.ord)
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptBlock,
                    "locale info strings are empty or unordered",
                ));
            }
            info.push(value);
        }
        if glosses.is_empty() && info.is_empty() {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "locale group has no localized text",
            ));
        }
        groups.push(LocaleGroup {
            targets,
            glosses,
            info,
        });
    }
    cursor.finish()?;
    Ok(LocaleEntry { seq, groups })
}

fn gunzip(compressed: &[u8], expected: usize, label: &str) -> Result<Vec<u8>> {
    let limit = expected.checked_add(1).ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            format!("{label} gzip decoded length overflows"),
        )
    })?;
    let decoder = GzDecoder::new(compressed);
    let mut bounded = decoder.take(limit as u64);
    let mut decoded = Vec::new();
    bounded.read_to_end(&mut decoded).map_err(|error| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            format!("{label} gzip decode failed: {error}"),
        )
    })?;
    if decoded.len() != expected {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!(
                "{label} gzip decoded {} bytes; expected {expected}",
                decoded.len()
            ),
        ));
    }
    if !bounded.into_inner().get_ref().is_empty() {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!("{label} gzip range has trailing compressed bytes"),
        ));
    }
    Ok(decoded)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn short_locale_prefix_is_rejected_without_panicking() {
        let result = LocaleStore::open(vec![0; 64], 64, &[0; 32], "en", 1);
        assert!(matches!(
            result,
            Err(KernelError {
                code: ErrorCode::InvalidHeader,
                ..
            })
        ));
    }

    #[test]
    fn localization_falls_back_per_field_and_keeps_entry_wide_groups() {
        let lexicon = LexiconEntry {
            seq: 7,
            forms: Vec::new(),
            senses: vec![
                LexiconSense {
                    ord: 0,
                    properties: Vec::new(),
                },
                LexiconSense {
                    ord: 1,
                    properties: Vec::new(),
                },
            ],
        };
        let text = |ord, value: &str| DictionaryGloss {
            ord,
            text: value.to_owned(),
        };
        let locale = LocaleEntry {
            seq: 7,
            groups: vec![
                LocaleGroup {
                    targets: vec![0],
                    glosses: vec![text(0, "吃")],
                    info: Vec::new(),
                },
                LocaleGroup {
                    targets: Vec::new(),
                    glosses: vec![text(0, "整词")],
                    info: Vec::new(),
                },
            ],
        };
        let fallback = LocaleEntry {
            seq: 7,
            groups: vec![
                LocaleGroup {
                    targets: vec![0],
                    glosses: vec![text(0, "eat")],
                    info: vec![text(0, "note")],
                },
                LocaleGroup {
                    targets: vec![1],
                    glosses: vec![text(0, "consume")],
                    info: Vec::new(),
                },
            ],
        };
        let entry = localize_entry(&lexicon, &locale, Some(&fallback)).unwrap();
        assert_eq!(entry.senses[0].glosses[0].text, "吃");
        assert_eq!(entry.senses[0].properties[0].text, "note");
        assert_eq!(entry.senses[1].glosses[0].text, "consume");
        assert_eq!(entry.senses[2].glosses[0].text, "整词");
    }

    #[test]
    fn entry_wide_fields_fall_back_independently() {
        let lexicon = LexiconEntry {
            seq: 1,
            forms: Vec::new(),
            senses: vec![LexiconSense {
                ord: 0,
                properties: Vec::new(),
            }],
        };
        let localized = LocaleEntry {
            seq: 1,
            groups: vec![LocaleGroup {
                targets: Vec::new(),
                glosses: vec![DictionaryGloss {
                    ord: 0,
                    text: "整词".to_owned(),
                }],
                info: Vec::new(),
            }],
        };
        let english = LocaleEntry {
            seq: 1,
            groups: vec![LocaleGroup {
                targets: Vec::new(),
                glosses: Vec::new(),
                info: vec![DictionaryGloss {
                    ord: 0,
                    text: "note".to_owned(),
                }],
            }],
        };
        let result = localize_entry(&lexicon, &localized, Some(&english)).unwrap();
        assert_eq!(result.senses[1].glosses[0].text, "整词");
        assert_eq!(result.senses[1].properties[0].text, "note");
    }

    #[test]
    fn localization_rejects_targets_absent_from_lexicon() {
        let lexicon = LexiconEntry {
            seq: 1,
            forms: Vec::new(),
            senses: vec![LexiconSense {
                ord: 0,
                properties: Vec::new(),
            }],
        };
        let locale = LocaleEntry {
            seq: 1,
            groups: vec![LocaleGroup {
                targets: vec![9],
                glosses: vec![DictionaryGloss {
                    ord: 0,
                    text: "invalid".to_owned(),
                }],
                info: Vec::new(),
            }],
        };
        assert!(matches!(
            localize_entry(&lexicon, &locale, None),
            Err(KernelError {
                code: ErrorCode::CorruptBlock,
                ..
            })
        ));
    }
}
