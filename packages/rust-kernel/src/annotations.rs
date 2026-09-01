use serde::Serialize;

mod generated_block;
#[cfg(test)]
mod strict_tests;

use generated_block::{DecodedBlock, decode_generated_block, root_location};

use crate::binary::{
    ByteSlice, align, assert_zero, checked_range, checked_table_end, crc32, magic, u16_at, u24_at,
    u32_at,
};
use crate::error::{ErrorCode, KernelError, Result};

const MAGIC: &[u8; 8] = b"IANAN001";
const VERSION: u16 = 4;
const HEADER_BYTES: usize = 184;
const BLOCK_BYTES: usize = 24;
const GENERATED_ROOT_BYTES: usize = 8;
const GENERATED_RECORD_BYTES: usize = 10;
const LOOKUP_ORDER_BYTES: usize = 4;

const ALIAS_BITS: u32 = 11;
const ALIAS_MAX: u32 = (1 << ALIAS_BITS) - 2;
const KEY_BITS: u32 = ALIAS_BITS * 2;
const KEY_MASK: u32 = (1 << KEY_BITS) - 1;
const GROUP_MASK: u32 = (1 << 18) - 1;
const VIA_NONE: u32 = 7;
const PROPERTY_NONE: u16 = 0xffff;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GeneratedFacts {
    pub n_kanji: Option<u8>,
    pub n_kana: Option<u8>,
    pub physical_group: Option<u32>,
    pub members: Option<Vec<GeneratedMember>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GeneratedMember {
    pub property: GeneratedProperty,
    pub member_ord: u8,
    pub prop_ord: u16,
    pub via_member_ord: Option<u8>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GeneratedProperty {
    pub pos_id: u8,
    #[serde(rename = "type")]
    pub kind: u8,
    pub negative: Option<bool>,
    pub formal: Option<bool>,
}

#[derive(Clone, Copy)]
struct GeneratedIndex {
    offset: usize,
    compressed: usize,
    uncompressed: usize,
    checksum: u32,
    roots: usize,
    orders: usize,
}

struct AnnotationBlockTotals {
    compressed: usize,
    splits: usize,
    hints: usize,
    uncompressed: usize,
    largest: usize,
}

pub struct AnalyzerAnnotations {
    bytes: ByteSlice,
    generated_data_offset: usize,
    generated_roots_offset: usize,
    generated_facts_offset: usize,
    generated_roots: usize,
    generated_records: usize,
    generated_fact_pairs: usize,
    generated_physical_groups: u32,
    generated_indexes: Vec<GeneratedIndex>,
    decoded: Vec<Option<DecodedBlock>>,
    decoded_blocks: usize,
    decoded_bytes: usize,
    decoded_records: usize,
}

impl AnalyzerAnnotations {
    pub(crate) fn open(bytes: ByteSlice) -> Result<Self> {
        if bytes.len() < HEADER_BYTES || !magic(&bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete IANAN001 header",
            ));
        }
        if u16_at(&bytes, 8, ErrorCode::InvalidHeader, "annotation version")? != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                "unsupported analyzer-annotations version",
            ));
        }
        if u16_at(
            &bytes,
            10,
            ErrorCode::InvalidHeader,
            "annotation header size",
        )? as usize
            != HEADER_BYTES
            || u32_at(
                &bytes,
                12,
                ErrorCode::InvalidHeader,
                "annotation total size",
            )? as usize
                != bytes.len()
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "annotation header size is invalid",
            ));
        }
        let mut header = bytes[..HEADER_BYTES].to_vec();
        header[16..20].fill(0);
        if crc32(&header)
            != u32_at(
                &bytes,
                16,
                ErrorCode::InvalidHeader,
                "annotation header checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "annotation header checksum does not match",
            ));
        }
        let blocks = field(&bytes, 24, "annotation block count")?;
        let splits = field(&bytes, 28, "annotation split count")?;
        let hints = field(&bytes, 32, "annotation hint count")?;
        let generated_blocks = field(&bytes, 52, "generated block count")?;
        let generated_roots = field(&bytes, 56, "generated root count")?;
        let generated_records = field(&bytes, 60, "generated record count")?;
        let generated_physical_groups = field(&bytes, 64, "generated physical group count")? as u32;
        let generated_fact_pairs = field(&bytes, 68, "generated fact-pair count")?;
        let lookup_order_records = field(&bytes, 128, "generated lookup-order count")?;
        let lookup_order_roots = field(&bytes, 132, "generated lookup-order root count")?;
        let lookup_order_max_rank = field(&bytes, 136, "generated lookup-order rank")?;
        let exception_surfaces = field(&bytes, 144, "lookup exception surface count")?;
        let exception_locators = field(&bytes, 148, "lookup exception locator count")?;
        if u32_at(
            &bytes,
            36,
            ErrorCode::InvalidHeader,
            "annotation block stride",
        )? as usize
            != BLOCK_BYTES
            || u32_at(
                &bytes,
                72,
                ErrorCode::InvalidHeader,
                "generated block stride",
            )? as usize
                != BLOCK_BYTES
            || u32_at(
                &bytes,
                76,
                ErrorCode::InvalidHeader,
                "generated root stride",
            )? as usize
                != GENERATED_ROOT_BYTES
            || u32_at(
                &bytes,
                80,
                ErrorCode::InvalidHeader,
                "generated record stride",
            )? as usize
                != GENERATED_RECORD_BYTES
            || u32_at(
                &bytes,
                120,
                ErrorCode::InvalidHeader,
                "generated target block bytes",
            )? != 256 * 1024
            || u32_at(&bytes, 140, ErrorCode::InvalidHeader, "lookup-order stride")? as usize
                != LOOKUP_ORDER_BYTES
            || u32_at(
                &bytes,
                160,
                ErrorCode::InvalidHeader,
                "lookup exception stride",
            )? != 16
            || u32_at(
                &bytes,
                164,
                ErrorCode::InvalidHeader,
                "lookup locator stride",
            )? != 8
            || lookup_order_max_rank > 0x3f
            || lookup_order_roots > generated_roots
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "annotation record stride or count is invalid",
            ));
        }
        let blocks_offset = field(&bytes, 40, "annotation blocks offset")?;
        let annotation_data_offset = field(&bytes, 44, "annotation data offset")?;
        let annotation_compressed_bytes = field(&bytes, 48, "annotation compressed bytes")?;
        let generated_blocks_offset = field(&bytes, 84, "generated blocks offset")?;
        let generated_roots_offset = field(&bytes, 88, "generated roots offset")?;
        let generated_facts_offset = field(&bytes, 92, "generated facts offset")?;
        let generated_data_offset = field(&bytes, 96, "generated data offset")?;
        let generated_compressed_bytes = field(&bytes, 100, "generated compressed bytes")?;
        let generated_uncompressed_bytes = field(&bytes, 104, "generated uncompressed bytes")?;
        let largest_generated_block = field(&bytes, 108, "largest generated block")?;
        let annotation_uncompressed_bytes = field(&bytes, 112, "annotation uncompressed bytes")?;
        let largest_annotation_block = field(&bytes, 116, "largest annotation block")?;
        let largest_generated_compressed =
            field(&bytes, 124, "largest generated compressed block")?;
        let exception_entries_offset = field(&bytes, 168, "lookup exception entries offset")?;
        let exception_locators_offset = field(&bytes, 172, "lookup exception locators offset")?;
        let exception_strings_offset = field(&bytes, 176, "lookup exception strings offset")?;
        let exception_string_bytes = field(&bytes, 180, "lookup exception string bytes")?;

        let expected_generated_blocks = checked_table_end(
            blocks_offset,
            blocks,
            BLOCK_BYTES,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "annotation block table",
        )?;
        let expected_generated_roots = checked_table_end(
            expected_generated_blocks,
            generated_blocks,
            BLOCK_BYTES,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "generated block table",
        )?;
        let expected_generated_facts = checked_table_end(
            expected_generated_roots,
            generated_roots,
            GENERATED_ROOT_BYTES,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "generated root table",
        )?;
        let generated_facts_end = checked_table_end(
            expected_generated_facts,
            generated_fact_pairs,
            2,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "generated fact table",
        )?;
        let expected_exception_entries = align(generated_facts_end, 8)?;
        let expected_exception_locators = checked_table_end(
            expected_exception_entries,
            exception_surfaces,
            16,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "lookup exception table",
        )?;
        let expected_exception_strings = checked_table_end(
            expected_exception_locators,
            exception_locators,
            8,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "lookup exception locator table",
        )?;
        let exception_strings_end = checked_table_end(
            expected_exception_strings,
            exception_string_bytes,
            1,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "lookup exception strings",
        )?;
        let expected_annotation_data = align(exception_strings_end, 8)?;
        let annotation_data_end = checked_table_end(
            annotation_data_offset,
            annotation_compressed_bytes,
            1,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "annotation compressed data",
        )?;
        let expected_generated_data = align(annotation_data_end, 8)?;
        let generated_data_end = checked_table_end(
            generated_data_offset,
            generated_compressed_bytes,
            1,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "generated compressed data",
        )?;
        if blocks_offset != HEADER_BYTES
            || generated_blocks_offset != expected_generated_blocks
            || generated_roots_offset != expected_generated_roots
            || generated_facts_offset != expected_generated_facts
            || exception_entries_offset != expected_exception_entries
            || exception_locators_offset != expected_exception_locators
            || exception_strings_offset != expected_exception_strings
            || annotation_data_offset != expected_annotation_data
            || generated_data_offset != expected_generated_data
            || align(generated_data_end, 8)? != bytes.len()
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "annotation layout is not canonical",
            ));
        }
        let index_bytes = annotation_data_offset
            .checked_sub(blocks_offset)
            .ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    "annotation index range is reversed",
                )
            })?;
        let index = checked_range(
            &bytes,
            blocks_offset,
            index_bytes,
            ErrorCode::CorruptIndex,
            "annotation index",
        )?;
        if crc32(index)
            != u32_at(
                &bytes,
                20,
                ErrorCode::CorruptIndex,
                "annotation index checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "annotation index checksum does not match",
            ));
        }
        validate_annotation_blocks(
            &bytes,
            blocks_offset,
            blocks,
            AnnotationBlockTotals {
                compressed: annotation_compressed_bytes,
                splits,
                hints,
                uncompressed: annotation_uncompressed_bytes,
                largest: largest_annotation_block,
            },
        )?;
        let mut generated_indexes = Vec::with_capacity(generated_blocks);
        let mut next_data = 0;
        let mut root_total = 0_usize;
        let mut uncompressed_total = 0_usize;
        let mut order_total = 0_usize;
        let mut maximum_block = 0;
        let mut maximum_compressed = 0;
        let mut previous_root = 0_u32;
        for block in 0..generated_blocks {
            let at = checked_table_end(
                generated_blocks_offset,
                block,
                BLOCK_BYTES,
                bytes.len(),
                ErrorCode::CorruptIndex,
                "generated block index",
            )?;
            let first_root = u32_at(&bytes, at, ErrorCode::CorruptIndex, "generated first root")?;
            let index = GeneratedIndex {
                offset: field(&bytes, at + 4, "generated data offset")?,
                compressed: field(&bytes, at + 8, "generated compressed length")?,
                uncompressed: field(&bytes, at + 12, "generated uncompressed length")?,
                checksum: u32_at(
                    &bytes,
                    at + 16,
                    ErrorCode::CorruptIndex,
                    "generated checksum",
                )?,
                roots: u16_at(
                    &bytes,
                    at + 20,
                    ErrorCode::CorruptIndex,
                    "generated root count",
                )? as usize,
                orders: u16_at(
                    &bytes,
                    at + 22,
                    ErrorCode::CorruptIndex,
                    "generated order count",
                )? as usize,
            };
            let block_end = index.offset.checked_add(index.compressed).ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    "generated block data range overflows",
                )
            })?;
            if index.roots == 0
                || (block > 0 && first_root <= previous_root)
                || index.offset != next_data
                || block_end > generated_compressed_bytes
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptIndex,
                    "generated block index is not canonical",
                ));
            }
            previous_root = first_root;
            next_data = block_end;
            root_total = root_total.checked_add(index.roots).ok_or_else(|| {
                KernelError::new(ErrorCode::CorruptIndex, "generated root total overflows")
            })?;
            uncompressed_total = uncompressed_total
                .checked_add(index.uncompressed)
                .ok_or_else(|| {
                    KernelError::new(
                        ErrorCode::CorruptIndex,
                        "generated decoded byte total overflows",
                    )
                })?;
            order_total = order_total.checked_add(index.orders).ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    "generated lookup-order total overflows",
                )
            })?;
            maximum_block = maximum_block.max(index.uncompressed);
            maximum_compressed = maximum_compressed.max(index.compressed);
            generated_indexes.push(index);
        }
        if next_data != generated_compressed_bytes
            || root_total != generated_roots
            || uncompressed_total != generated_uncompressed_bytes
            || order_total != lookup_order_records
            || maximum_block != largest_generated_block
            || maximum_compressed != largest_generated_compressed
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "generated block totals disagree with the header",
            ));
        }
        validate_generated_roots(
            &bytes,
            generated_roots_offset,
            generated_roots,
            &generated_indexes,
            generated_blocks_offset,
        )?;
        assert_zero(
            &bytes,
            generated_facts_end,
            exception_entries_offset,
            ErrorCode::CorruptIndex,
            "generated fact padding",
        )?;
        assert_zero(
            &bytes,
            annotation_data_end,
            generated_data_offset,
            ErrorCode::CorruptIndex,
            "annotation data padding",
        )?;
        assert_zero(
            &bytes,
            generated_data_end,
            bytes.len(),
            ErrorCode::CorruptIndex,
            "annotation trailing padding",
        )?;
        checked_table_end(
            generated_facts_offset,
            generated_fact_pairs,
            2,
            bytes.len(),
            ErrorCode::CorruptIndex,
            "generated facts",
        )?;

        Ok(Self {
            bytes,
            generated_data_offset,
            generated_roots_offset,
            generated_facts_offset,
            generated_roots,
            generated_records,
            generated_fact_pairs,
            generated_physical_groups,
            decoded: (0..generated_blocks).map(|_| None).collect(),
            generated_indexes,
            decoded_blocks: 0,
            decoded_bytes: 0,
            decoded_records: 0,
        })
    }

    pub fn generated(&mut self, root_seq: u32, aliases: &[u16]) -> Result<Option<GeneratedFacts>> {
        let Some(block) = self.block_for_root(root_seq)? else {
            return Ok(None);
        };
        let wanted = generated_key(aliases)?;
        self.decode_block(block)?;
        let decoded = self.decoded[block].as_ref().ok_or_else(|| {
            KernelError::new(
                ErrorCode::Internal,
                "generated block was not retained after decoding",
            )
        })?;
        let Some((first, count, _, _)) = root_location(decoded, root_seq)? else {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "generated root index disagrees with decoded block",
            ));
        };
        let mut low = 0;
        let mut high = count;
        while low < high {
            let middle = (low + high) / 2;
            let key = u32_at(
                &decoded.bytes,
                decoded.records_offset + (first + middle) * GENERATED_RECORD_BYTES,
                ErrorCode::CorruptBlock,
                "generated record",
            )? & KEY_MASK;
            if key < wanted {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        if low >= count {
            return Ok(None);
        }
        let at = decoded.records_offset + (first + low) * GENERATED_RECORD_BYTES;
        if u32_at(
            &decoded.bytes,
            at,
            ErrorCode::CorruptBlock,
            "generated record",
        )? & KEY_MASK
            != wanted
        {
            return Ok(None);
        }
        decode_fact(
            &self.bytes,
            self.generated_facts_offset,
            decoded,
            first + low,
            count - low,
            wanted,
        )
        .map(Some)
    }

    pub fn preload_all_generated(&mut self) -> Result<()> {
        for block in 0..self.generated_indexes.len() {
            self.decode_block(block)?;
        }
        if self.decoded_records != self.generated_records {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "generated record totals disagree with the header",
            ));
        }
        Ok(())
    }

    pub fn decoded_bytes(&self) -> usize {
        self.decoded_bytes
    }

    pub fn decoded_block_count(&self) -> usize {
        self.decoded_blocks
    }

    pub fn generated_block_count(&self) -> usize {
        self.generated_indexes.len()
    }

    fn decode_block(&mut self, block: usize) -> Result<()> {
        if self.decoded.get(block).is_some_and(Option::is_some) {
            return Ok(());
        }
        let index = *self.generated_indexes.get(block).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "generated block index is out of range",
            )
        })?;
        let offset = self
            .generated_data_offset
            .checked_add(index.offset)
            .ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    "generated block data offset overflows",
                )
            })?;
        let compressed = checked_range(
            &self.bytes,
            offset,
            index.compressed,
            ErrorCode::CorruptBlock,
            "generated compressed block",
        )?;
        let decoded = decode_generated_block(
            compressed,
            index,
            block,
            self.generated_records,
            self.generated_fact_pairs,
            self.generated_physical_groups,
        )?;
        let decoded_bytes = self
            .decoded_bytes
            .checked_add(decoded.bytes.len())
            .ok_or_else(|| {
                KernelError::new(ErrorCode::CorruptBlock, "generated decoded bytes overflow")
            })?;
        let decoded_records = checked_decoded_records(
            self.decoded_records,
            decoded.records,
            self.generated_records,
        )?;
        let decoded_blocks = self.decoded_blocks.checked_add(1).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptBlock,
                "generated decoded block count overflows",
            )
        })?;
        self.decoded[block] = Some(decoded);
        self.decoded_bytes = decoded_bytes;
        self.decoded_records = decoded_records;
        self.decoded_blocks = decoded_blocks;
        Ok(())
    }

    fn block_for_root(&self, root_seq: u32) -> Result<Option<usize>> {
        let mut low = 0;
        let mut high = self.generated_roots;
        while low < high {
            let middle = low + (high - low) / 2;
            let at = checked_table_end(
                self.generated_roots_offset,
                middle,
                GENERATED_ROOT_BYTES,
                self.bytes.len(),
                ErrorCode::CorruptIndex,
                "generated root index",
            )?;
            if u32_at(
                &self.bytes,
                at,
                ErrorCode::CorruptIndex,
                "generated root index",
            )? < root_seq
            {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        if low >= self.generated_roots {
            return Ok(None);
        }
        let at = checked_table_end(
            self.generated_roots_offset,
            low,
            GENERATED_ROOT_BYTES,
            self.bytes.len(),
            ErrorCode::CorruptIndex,
            "generated root index",
        )?;
        if u32_at(
            &self.bytes,
            at,
            ErrorCode::CorruptIndex,
            "generated root index",
        )? != root_seq
        {
            return Ok(None);
        }
        Ok(Some(u16_at(
            &self.bytes,
            at + 4,
            ErrorCode::CorruptIndex,
            "generated block index",
        )? as usize))
    }
}

fn checked_decoded_records(current: usize, added: usize, declared: usize) -> Result<usize> {
    let total = current.checked_add(added).ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            "generated decoded records overflow",
        )
    })?;
    if total > declared {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "generated decoded record total exceeds the header",
        ));
    }
    Ok(total)
}

fn decode_fact(
    bytes: &[u8],
    generated_facts_offset: usize,
    block: &DecodedBlock,
    first: usize,
    count: usize,
    wanted: u32,
) -> Result<GeneratedFacts> {
    let mut members = Vec::new();
    let mut fact_code = None;
    let mut physical_group = None;
    let mut count_only = false;
    for index in 0..count {
        let at = block.records_offset + (first + index) * GENERATED_RECORD_BYTES;
        let stored_key = u32_at(
            &block.bytes,
            at,
            ErrorCode::CorruptBlock,
            "generated record key",
        )?;
        if stored_key & KEY_MASK != wanted {
            break;
        }
        let fact = block.bytes[at + 4];
        let physical = u24_at(
            &block.bytes,
            at + 5,
            ErrorCode::CorruptBlock,
            "generated physical identity",
        )?;
        let property = u16_at(
            &block.bytes,
            at + 8,
            ErrorCode::CorruptBlock,
            "generated property",
        )?;
        if fact_code.is_none() {
            fact_code = Some(fact);
            let group = physical & GROUP_MASK;
            physical_group = (group != 0).then_some(group);
        }
        if property == PROPERTY_NONE {
            count_only = true;
            continue;
        }
        let negative = ((property >> 11) & 3) as u8;
        let formal = ((property >> 13) & 3) as u8;
        let via = ((physical >> 21) & 7) as u8;
        members.push(GeneratedMember {
            property: GeneratedProperty {
                pos_id: (property & 31) as u8,
                kind: ((property >> 5) & 63) as u8,
                negative: tri(negative)?,
                formal: tri(formal)?,
            },
            member_ord: ((physical >> 18) & 7) as u8,
            prop_ord: (stored_key >> KEY_BITS) as u16,
            via_member_ord: (u32::from(via) != VIA_NONE).then_some(via),
        });
    }
    let fact = fact_code.unwrap_or(0) as usize;
    let (n_kanji, n_kana) = if fact == 0 {
        (None, None)
    } else {
        let at = generated_facts_offset + (fact - 1) * 2;
        (Some(bytes[at]), Some(bytes[at + 1]))
    };
    Ok(GeneratedFacts {
        n_kanji,
        n_kana,
        physical_group,
        members: (!count_only).then_some(members),
    })
}

fn field(bytes: &[u8], offset: usize, label: &str) -> Result<usize> {
    Ok(u32_at(bytes, offset, ErrorCode::InvalidHeader, label)? as usize)
}

fn validate_annotation_blocks(
    bytes: &[u8],
    offset: usize,
    blocks: usize,
    expected: AnnotationBlockTotals,
) -> Result<()> {
    let mut previous_seq = 0;
    let mut next_data = 0;
    let mut splits = 0_usize;
    let mut hints = 0_usize;
    let mut uncompressed = 0_usize;
    let mut largest = 0;
    for block in 0..blocks {
        let at = checked_table_end(
            offset,
            block,
            BLOCK_BYTES,
            bytes.len(),
            ErrorCode::CorruptIndex,
            "annotation block index",
        )?;
        let seq = u32_at(bytes, at, ErrorCode::CorruptIndex, "annotation sequence")?;
        let data = field(bytes, at + 4, "annotation data offset")?;
        let compressed = field(bytes, at + 8, "annotation compressed length")?;
        let decoded = field(bytes, at + 12, "annotation decoded length")?;
        let data_end = data.checked_add(compressed).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "annotation block data range overflows",
            )
        })?;
        if (block > 0 && seq <= previous_seq) || data != next_data || data_end > expected.compressed
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "annotation block index is not canonical",
            ));
        }
        previous_seq = seq;
        next_data = data_end;
        splits = splits
            .checked_add(u16_at(
                bytes,
                at + 20,
                ErrorCode::CorruptIndex,
                "annotation split count",
            )? as usize)
            .ok_or_else(|| {
                KernelError::new(ErrorCode::CorruptIndex, "annotation split total overflows")
            })?;
        hints = hints
            .checked_add(u16_at(
                bytes,
                at + 22,
                ErrorCode::CorruptIndex,
                "annotation hint count",
            )? as usize)
            .ok_or_else(|| {
                KernelError::new(ErrorCode::CorruptIndex, "annotation hint total overflows")
            })?;
        uncompressed = uncompressed.checked_add(decoded).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "annotation decoded byte total overflows",
            )
        })?;
        largest = largest.max(decoded);
    }
    if next_data != expected.compressed
        || splits != expected.splits
        || hints != expected.hints
        || uncompressed != expected.uncompressed
        || largest != expected.largest
    {
        return Err(KernelError::new(
            ErrorCode::CorruptIndex,
            "annotation block totals disagree with the header",
        ));
    }
    Ok(())
}

fn validate_generated_roots(
    bytes: &[u8],
    offset: usize,
    roots: usize,
    indexes: &[GeneratedIndex],
    blocks_offset: usize,
) -> Result<()> {
    let mut seen = vec![0_usize; indexes.len()];
    let mut previous_seq = 0;
    let mut previous_block = 0;
    for root in 0..roots {
        let at = checked_table_end(
            offset,
            root,
            GENERATED_ROOT_BYTES,
            bytes.len(),
            ErrorCode::CorruptIndex,
            "generated root index",
        )?;
        let seq = u32_at(bytes, at, ErrorCode::CorruptIndex, "generated root")?;
        let block = u16_at(bytes, at + 4, ErrorCode::CorruptIndex, "generated block")? as usize;
        let reserved = u16_at(
            bytes,
            at + 6,
            ErrorCode::CorruptIndex,
            "generated root reserved field",
        )?;
        if block >= indexes.len()
            || reserved != 0
            || (root > 0 && seq <= previous_seq)
            || (root > 0 && block < previous_block)
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "generated root index is not canonical",
            ));
        }
        if seen[block] == 0 {
            let block_at = checked_table_end(
                blocks_offset,
                block,
                BLOCK_BYTES,
                bytes.len(),
                ErrorCode::CorruptIndex,
                "generated block index",
            )?;
            let first = u32_at(
                bytes,
                block_at,
                ErrorCode::CorruptIndex,
                "generated first root",
            )?;
            if first != seq {
                return Err(KernelError::new(
                    ErrorCode::CorruptIndex,
                    "generated root and block indexes disagree",
                ));
            }
        }
        seen[block] += 1;
        previous_seq = seq;
        previous_block = block;
    }
    if seen
        .iter()
        .zip(indexes)
        .any(|(actual, expected)| *actual != expected.roots)
    {
        return Err(KernelError::new(
            ErrorCode::CorruptIndex,
            "generated roots do not cover blocks",
        ));
    }
    Ok(())
}

fn generated_key(aliases: &[u16]) -> Result<u32> {
    if aliases.len() != 1 && aliases.len() != 2 {
        return Err(KernelError::new(
            ErrorCode::OutOfRange,
            "generated aliases require one or two values",
        ));
    }
    let first = u32::from(aliases[0]);
    let second = aliases.get(1).map(|value| u32::from(*value));
    if first > ALIAS_MAX || second.is_some_and(|value| value > ALIAS_MAX) {
        return Err(KernelError::new(
            ErrorCode::OutOfRange,
            "generated alias is out of range",
        ));
    }
    let key = (first << ALIAS_BITS) | second.map_or(0, |value| value + 1);
    if key == KEY_MASK {
        return Err(KernelError::new(
            ErrorCode::OutOfRange,
            "generated aliases collide with the direct sentinel",
        ));
    }
    Ok(key)
}

fn tri(code: u8) -> Result<Option<bool>> {
    match code {
        0 => Ok(Some(false)),
        1 => Ok(Some(true)),
        2 => Ok(None),
        _ => Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!("invalid generated tri-state {code}"),
        )),
    }
}
