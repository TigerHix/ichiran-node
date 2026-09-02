use crate::binary::{checked_table_end, u16_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};

use super::ordinary_block::AnnotationIndex;
use super::{AnalyzerAnnotations, BLOCK_BYTES, GENERATED_ROOT_BYTES};

#[derive(Clone, Copy)]
pub(super) struct GeneratedIndex {
    pub(super) offset: usize,
    pub(super) compressed: usize,
    pub(super) uncompressed: usize,
    pub(super) checksum: u32,
    pub(super) roots: usize,
    pub(super) orders: usize,
}

pub(super) struct AnnotationBlockTotals {
    pub(super) compressed: usize,
    pub(super) splits: usize,
    pub(super) hints: usize,
    pub(super) uncompressed: usize,
    pub(super) largest: usize,
}

pub(super) fn field(bytes: &[u8], offset: usize, label: &str) -> Result<usize> {
    Ok(u32_at(bytes, offset, ErrorCode::InvalidHeader, label)? as usize)
}

pub(super) fn validate_annotation_blocks(
    bytes: &[u8],
    offset: usize,
    blocks: usize,
    expected: AnnotationBlockTotals,
) -> Result<Vec<AnnotationIndex>> {
    let mut indexes = Vec::with_capacity(blocks);
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
        let index = AnnotationIndex {
            seq: u32_at(bytes, at, ErrorCode::CorruptIndex, "annotation sequence")?,
            offset: field(bytes, at + 4, "annotation data offset")?,
            compressed: field(bytes, at + 8, "annotation compressed length")?,
            uncompressed: field(bytes, at + 12, "annotation decoded length")?,
            checksum: u32_at(
                bytes,
                at + 16,
                ErrorCode::CorruptIndex,
                "annotation checksum",
            )?,
            splits: u16_at(
                bytes,
                at + 20,
                ErrorCode::CorruptIndex,
                "annotation split count",
            )? as usize,
            hints: u16_at(
                bytes,
                at + 22,
                ErrorCode::CorruptIndex,
                "annotation hint count",
            )? as usize,
        };
        let data_end = index.offset.checked_add(index.compressed).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "annotation block data range overflows",
            )
        })?;
        if (block > 0 && index.seq <= previous_seq)
            || index.offset != next_data
            || data_end > expected.compressed
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "annotation block index is not canonical",
            ));
        }
        previous_seq = index.seq;
        next_data = data_end;
        splits = splits.checked_add(index.splits).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptIndex, "annotation split total overflows")
        })?;
        hints = hints.checked_add(index.hints).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptIndex, "annotation hint total overflows")
        })?;
        uncompressed = uncompressed
            .checked_add(index.uncompressed)
            .ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptIndex,
                    "annotation decoded byte total overflows",
                )
            })?;
        largest = largest.max(index.uncompressed);
        indexes.push(index);
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
    Ok(indexes)
}

pub(super) fn validate_generated_roots(
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

impl AnalyzerAnnotations {
    pub(super) fn block_for_root(&self, root_seq: u32) -> Result<Option<usize>> {
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
