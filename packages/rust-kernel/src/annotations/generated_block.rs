use std::io::Read;

use flate2::bufread::GzDecoder;

use super::{
    ALIAS_BITS, ALIAS_MAX, GENERATED_RECORD_BYTES, GROUP_MASK, GeneratedIndex, KEY_MASK,
    LOOKUP_ORDER_BYTES, PROPERTY_NONE,
};
use crate::binary::{checked_table_end, crc32, u16_at, u24_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};

#[derive(Debug)]
pub(super) struct DecodedBlock {
    pub(super) bytes: Vec<u8>,
    roots: usize,
    pub(super) records: usize,
    pub(super) records_offset: usize,
}

pub(super) fn decode_generated_block(
    compressed: &[u8],
    index: GeneratedIndex,
    block_index: usize,
    total_record_limit: usize,
    fact_pairs: usize,
    physical_groups: u32,
) -> Result<DecodedBlock> {
    let bytes = gunzip(compressed, index.uncompressed)?;
    if crc32(&bytes) != index.checksum {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!("generated block {block_index} checksum does not match"),
        ));
    }
    validate_generated_block(
        bytes,
        index.roots,
        index.orders,
        total_record_limit,
        fact_pairs,
        physical_groups,
    )
}

fn validate_generated_block(
    bytes: Vec<u8>,
    expected_roots: usize,
    expected_orders: usize,
    total_record_limit: usize,
    fact_pairs: usize,
    physical_groups: u32,
) -> Result<DecodedBlock> {
    if bytes.len() < 12 {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "generated block is truncated",
        ));
    }
    let roots = u32_at(
        &bytes,
        0,
        ErrorCode::CorruptBlock,
        "generated block root count",
    )? as usize;
    let records = u32_at(
        &bytes,
        4,
        ErrorCode::CorruptBlock,
        "generated block record count",
    )? as usize;
    let orders = u32_at(
        &bytes,
        8,
        ErrorCode::CorruptBlock,
        "generated block order count",
    )? as usize;
    let records_offset = checked_table_end(
        12,
        roots,
        20,
        bytes.len(),
        ErrorCode::CorruptBlock,
        "generated root table",
    )?;
    let orders_offset = checked_table_end(
        records_offset,
        records,
        GENERATED_RECORD_BYTES,
        bytes.len(),
        ErrorCode::CorruptBlock,
        "generated record table",
    )?;
    let block_end = checked_table_end(
        orders_offset,
        orders,
        LOOKUP_ORDER_BYTES,
        bytes.len(),
        ErrorCode::CorruptBlock,
        "generated lookup-order table",
    )?;
    if roots != expected_roots
        || orders != expected_orders
        || records > total_record_limit
        || block_end != bytes.len()
    {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "generated block dimensions are invalid",
        ));
    }
    let block = DecodedBlock {
        bytes,
        roots,
        records,
        records_offset,
    };
    let mut next_record = 0;
    let mut next_order = 0;
    let mut previous_seq = 0;
    for root in 0..roots {
        let at = table_entry(12, root, 20, block.bytes.len(), "generated root table")?;
        let seq = u32_at(
            &block.bytes,
            at,
            ErrorCode::CorruptBlock,
            "generated decoded root",
        )?;
        let first = u32_at(
            &block.bytes,
            at + 4,
            ErrorCode::CorruptBlock,
            "generated first record",
        )? as usize;
        let count = u32_at(
            &block.bytes,
            at + 8,
            ErrorCode::CorruptBlock,
            "generated record count",
        )? as usize;
        let first_order = u32_at(
            &block.bytes,
            at + 12,
            ErrorCode::CorruptBlock,
            "generated first order",
        )? as usize;
        let order_count = u32_at(
            &block.bytes,
            at + 16,
            ErrorCode::CorruptBlock,
            "generated order count",
        )? as usize;
        if (root > 0 && seq <= previous_seq)
            || first != next_record
            || first_order != next_order
            || (count == 0 && order_count == 0)
        {
            return Err(KernelError::new(
                ErrorCode::CorruptBlock,
                "generated decoded roots are not canonical",
            ));
        }
        let mut previous_key = None;
        for record in 0..count {
            let record_at = table_entry(
                records_offset,
                first.checked_add(record).ok_or_else(|| {
                    KernelError::new(ErrorCode::CorruptBlock, "generated record index overflows")
                })?,
                GENERATED_RECORD_BYTES,
                block.bytes.len(),
                "generated record table",
            )?;
            let stored_key = u32_at(
                &block.bytes,
                record_at,
                ErrorCode::CorruptBlock,
                "generated record",
            )?;
            let key = stored_key & KEY_MASK;
            let fact = block.bytes[record_at + 4] as usize;
            let physical = u24_at(
                &block.bytes,
                record_at + 5,
                ErrorCode::CorruptBlock,
                "generated record",
            )?;
            let property = u16_at(
                &block.bytes,
                record_at + 8,
                ErrorCode::CorruptBlock,
                "generated record",
            )?;
            let first_alias = key >> ALIAS_BITS;
            let second_code = key & ((1 << ALIAS_BITS) - 1);
            if previous_key.is_some_and(|prior| key < prior)
                || first_alias > ALIAS_MAX
                || second_code > ALIAS_MAX + 1
                || fact > fact_pairs
                || (physical & GROUP_MASK) > physical_groups
                || (property != PROPERTY_NONE
                    && (((property >> 11) & 3) > 2
                        || ((property >> 13) & 3) > 2
                        || property & 0x8000 != 0))
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptBlock,
                    "generated record is not canonical",
                ));
            }
            previous_key = Some(key);
        }
        let mut previous_order = None;
        for order in 0..order_count {
            let order_at = table_entry(
                orders_offset,
                first_order.checked_add(order).ok_or_else(|| {
                    KernelError::new(
                        ErrorCode::CorruptBlock,
                        "generated lookup-order index overflows",
                    )
                })?,
                LOOKUP_ORDER_BYTES,
                block.bytes.len(),
                "generated lookup-order table",
            )?;
            let packed = u32_at(
                &block.bytes,
                order_at,
                ErrorCode::CorruptBlock,
                "generated lookup order",
            )?;
            let key = packed & KEY_MASK;
            if packed >> 28 != 0 || previous_order.is_some_and(|prior| key <= prior) {
                return Err(KernelError::new(
                    ErrorCode::CorruptBlock,
                    "generated lookup order is not canonical",
                ));
            }
            previous_order = Some(key);
        }
        previous_seq = seq;
        next_record = next_record.checked_add(count).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptBlock, "generated record count overflows")
        })?;
        next_order = next_order.checked_add(order_count).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptBlock,
                "generated lookup-order count overflows",
            )
        })?;
    }
    if next_record != records || next_order != orders {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "generated block tables are not covered",
        ));
    }
    Ok(block)
}

pub(super) fn root_location(
    block: &DecodedBlock,
    root_seq: u32,
) -> Result<Option<(usize, usize, usize, usize)>> {
    let mut low = 0;
    let mut high = block.roots;
    while low < high {
        let middle = low + (high - low) / 2;
        let at = table_entry(12, middle, 20, block.bytes.len(), "generated root table")?;
        let seq = u32_at(
            &block.bytes,
            at,
            ErrorCode::CorruptBlock,
            "generated decoded root",
        )?;
        if seq < root_seq {
            low = middle + 1;
        } else {
            high = middle;
        }
    }
    if low >= block.roots {
        return Ok(None);
    }
    let at = table_entry(12, low, 20, block.bytes.len(), "generated root table")?;
    if u32_at(
        &block.bytes,
        at,
        ErrorCode::CorruptBlock,
        "generated decoded root",
    )? != root_seq
    {
        return Ok(None);
    }
    Ok(Some((
        u32_at(
            &block.bytes,
            at + 4,
            ErrorCode::CorruptBlock,
            "generated first record",
        )? as usize,
        u32_at(
            &block.bytes,
            at + 8,
            ErrorCode::CorruptBlock,
            "generated record count",
        )? as usize,
        u32_at(
            &block.bytes,
            at + 12,
            ErrorCode::CorruptBlock,
            "generated first order",
        )? as usize,
        u32_at(
            &block.bytes,
            at + 16,
            ErrorCode::CorruptBlock,
            "generated order count",
        )? as usize,
    )))
}

fn table_entry(
    offset: usize,
    index: usize,
    stride: usize,
    total: usize,
    label: &str,
) -> Result<usize> {
    checked_table_end(offset, index, stride, total, ErrorCode::CorruptBlock, label)
}

fn gunzip(compressed: &[u8], expected: usize) -> Result<Vec<u8>> {
    let limit = expected.checked_add(1).ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            "generated gzip decoded length overflows",
        )
    })?;
    let limit = u64::try_from(limit).map_err(|_| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            "generated gzip decoded length exceeds the reader limit",
        )
    })?;
    let decoder = GzDecoder::new(compressed);
    let mut bounded = decoder.take(limit);
    let mut decoded = Vec::new();
    bounded.read_to_end(&mut decoded).map_err(|error| {
        KernelError::new(
            ErrorCode::CorruptBlock,
            format!("gzip decode failed: {error}"),
        )
    })?;
    if decoded.len() != expected {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            format!("gzip decoded {} bytes; expected {expected}", decoded.len()),
        ));
    }
    if !bounded.into_inner().into_inner().is_empty() {
        return Err(KernelError::new(
            ErrorCode::CorruptBlock,
            "generated gzip has trailing compressed bytes",
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

    #[test]
    fn generated_gzip_rejects_oversized_expansion_at_expected_plus_one() {
        let compressed = gzip(&vec![0; 1024 * 1024]);
        let error = gunzip(&compressed, 16).unwrap_err();

        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(error.message, "gzip decoded 17 bytes; expected 16");
    }

    #[test]
    fn generated_gzip_rejects_expected_length_overflow() {
        let error = gunzip(&[], usize::MAX).unwrap_err();

        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(error.message, "generated gzip decoded length overflows");
    }

    #[test]
    fn generated_gzip_rejects_trailing_compressed_bytes() {
        let mut compressed = gzip(b"generated");
        compressed.push(0);
        let error = gunzip(&compressed, 9).unwrap_err();

        assert_eq!(error.code, ErrorCode::CorruptBlock);
        assert_eq!(
            error.message,
            "generated gzip has trailing compressed bytes"
        );
    }

    #[test]
    fn generated_block_rejects_overflowing_dimensions() {
        let mut bytes = vec![0; 12];
        bytes[..4].copy_from_slice(&u32::MAX.to_le_bytes());
        let error = validate_generated_block(bytes, u32::MAX as usize, 0, 0, 0, 0).unwrap_err();

        assert_eq!(error.code, ErrorCode::CorruptBlock);
    }
}
