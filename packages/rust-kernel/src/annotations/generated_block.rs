use std::io::Read;

use flate2::bufread::GzDecoder;

use super::{
    ALIAS_BITS, ALIAS_MAX, GENERATED_RECORD_BYTES, GROUP_MASK, GeneratedFacts, GeneratedIndex,
    GeneratedMember, GeneratedProperty, KEY_BITS, KEY_MASK, LOOKUP_ORDER_BYTES, PROPERTY_NONE,
    VIA_NONE,
};
use crate::binary::{checked_table_end, crc32, u16_at, u24_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};

#[derive(Debug)]
pub(super) struct DecodedBlock {
    pub(super) bytes: Vec<u8>,
    roots: usize,
    pub(super) records: usize,
    pub(super) records_offset: usize,
    orders_offset: usize,
    pub(super) order_roots: usize,
    pub(super) maximum_order_rank: u8,
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
        orders_offset,
        order_roots: 0,
        maximum_order_rank: 0,
    };
    let mut block = block;
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
        let mut previous_member_ord = 0;
        let mut previous_prop_ord = 0;
        let mut previous_via = VIA_NONE;
        let mut previous_property = 0;
        let mut semantic_fact = 0;
        let mut semantic_group = 0;
        let mut semantic_count_only = false;
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
            let prop_ord = stored_key >> KEY_BITS;
            let group = physical & GROUP_MASK;
            let member_ord = (physical >> 18) & 7;
            let via = (physical >> 21) & 7;
            let count_only = property == PROPERTY_NONE;
            let first_alias = key >> ALIAS_BITS;
            let second_code = key & ((1 << ALIAS_BITS) - 1);
            let same_semantic = previous_key == Some(key);
            let via_order = if via == VIA_NONE { 0 } else { via + 1 };
            let previous_via_order = if previous_via == VIA_NONE {
                0
            } else {
                previous_via + 1
            };
            let canonical_member_order = !same_semantic
                || member_ord > previous_member_ord
                || (member_ord == previous_member_ord && prop_ord > previous_prop_ord)
                || (member_ord == previous_member_ord
                    && prop_ord == previous_prop_ord
                    && via_order > previous_via_order)
                || (member_ord == previous_member_ord
                    && prop_ord == previous_prop_ord
                    && via_order == previous_via_order
                    && property > previous_property);
            if previous_key.is_some_and(|prior| key < prior)
                || !canonical_member_order
                || first_alias > ALIAS_MAX
                || second_code > ALIAS_MAX + 1
                || fact > fact_pairs
                || group > physical_groups
                || (same_semantic
                    && (fact != semantic_fact
                        || group != semantic_group
                        || semantic_count_only
                        || count_only))
                || (count_only && (fact == 0 || physical != 0 || prop_ord != 0))
                || (!count_only
                    && (member_ord > 6
                        || ((property >> 11) & 3) > 2
                        || ((property >> 13) & 3) > 2
                        || property & 0x8000 != 0))
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptBlock,
                    "generated record is not canonical",
                ));
            }
            if !same_semantic {
                semantic_fact = fact;
                semantic_group = group;
                semantic_count_only = count_only;
            }
            previous_key = Some(key);
            previous_member_ord = member_ord;
            previous_prop_ord = prop_ord;
            previous_via = via;
            previous_property = property;
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
            let first_alias = key >> ALIAS_BITS;
            let second_code = key & ((1 << ALIAS_BITS) - 1);
            if packed >> 28 != 0
                || previous_order.is_some_and(|prior| key <= prior)
                || (key != KEY_MASK && (first_alias > ALIAS_MAX || second_code > ALIAS_MAX + 1))
            {
                return Err(KernelError::new(
                    ErrorCode::CorruptBlock,
                    "generated lookup order is not canonical",
                ));
            }
            block.maximum_order_rank = block
                .maximum_order_rank
                .max(((packed >> KEY_BITS) & 0x3f) as u8);
            previous_order = Some(key);
        }
        if order_count > 0 {
            block.order_roots = block.order_roots.checked_add(1).ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptBlock,
                    "generated lookup-order root count overflows",
                )
            })?;
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

pub(super) fn generated_order(
    block: &DecodedBlock,
    root_seq: u32,
    wanted: u32,
) -> Result<Option<u8>> {
    let Some((_, _, first, count)) = root_location(block, root_seq)? else {
        return Err(KernelError::new(
            ErrorCode::CorruptIndex,
            "generated root index disagrees with decoded block",
        ));
    };
    let mut low = 0;
    let mut high = count;
    while low < high {
        let middle = low + (high - low) / 2;
        let at = table_entry(
            block.orders_offset,
            first.checked_add(middle).ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptBlock,
                    "generated lookup-order index overflows",
                )
            })?,
            LOOKUP_ORDER_BYTES,
            block.bytes.len(),
            "generated lookup-order table",
        )?;
        let key = u32_at(
            &block.bytes,
            at,
            ErrorCode::CorruptBlock,
            "generated lookup order",
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
    let at = table_entry(
        block.orders_offset,
        first.checked_add(low).ok_or_else(|| {
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
        at,
        ErrorCode::CorruptBlock,
        "generated lookup order",
    )?;
    Ok((packed & KEY_MASK == wanted).then_some(((packed >> KEY_BITS) & 0x3f) as u8))
}

pub(super) fn generated_key(aliases: &[u16]) -> Result<u32> {
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

pub(super) fn decode_fact(
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

    fn put_u32(bytes: &mut [u8], at: usize, value: u32) {
        bytes[at..at + 4].copy_from_slice(&value.to_le_bytes());
    }

    fn canonical_block() -> Vec<u8> {
        let mut bytes = vec![0; 12 + 20 + GENERATED_RECORD_BYTES + LOOKUP_ORDER_BYTES];
        put_u32(&mut bytes, 0, 1);
        put_u32(&mut bytes, 4, 1);
        put_u32(&mut bytes, 8, 1);
        put_u32(&mut bytes, 12, 10);
        put_u32(&mut bytes, 16, 0);
        put_u32(&mut bytes, 20, 1);
        put_u32(&mut bytes, 24, 0);
        put_u32(&mut bytes, 28, 1);
        put_u32(&mut bytes, 32, 0);
        bytes[36] = 1;
        let physical = VIA_NONE << 21;
        bytes[37] = physical as u8;
        bytes[38] = (physical >> 8) as u8;
        bytes[39] = (physical >> 16) as u8;
        bytes[40..42].copy_from_slice(&0x5000_u16.to_le_bytes());
        put_u32(&mut bytes, 42, KEY_MASK);
        bytes
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

    #[test]
    fn generated_block_accepts_canonical_members_and_orders() {
        let block = validate_generated_block(canonical_block(), 1, 1, 1, 1, 0).unwrap();
        assert_eq!(block.order_roots, 1);
        assert_eq!(block.maximum_order_rank, 0);
        assert_eq!(generated_order(&block, 10, KEY_MASK).unwrap(), Some(0));
        assert_eq!(generated_order(&block, 10, 0).unwrap(), None);
    }

    #[test]
    fn generated_block_rejects_member_ordinal_seven() {
        let mut bytes = canonical_block();
        let physical = (VIA_NONE << 21) | (7 << 18);
        bytes[37] = physical as u8;
        bytes[38] = (physical >> 8) as u8;
        bytes[39] = (physical >> 16) as u8;
        let error = validate_generated_block(bytes, 1, 1, 1, 1, 0).unwrap_err();
        assert_eq!(error.message, "generated record is not canonical");
    }

    #[test]
    fn generated_block_rejects_noncanonical_count_only_record() {
        let mut bytes = canonical_block();
        bytes[40..42].copy_from_slice(&PROPERTY_NONE.to_le_bytes());
        let error = validate_generated_block(bytes, 1, 1, 1, 1, 0).unwrap_err();
        assert_eq!(error.message, "generated record is not canonical");
    }

    #[test]
    fn generated_block_rejects_lookup_order_reserved_bits() {
        let mut bytes = canonical_block();
        put_u32(&mut bytes, 42, KEY_MASK | (1 << 28));
        let error = validate_generated_block(bytes, 1, 1, 1, 1, 0).unwrap_err();
        assert_eq!(error.message, "generated lookup order is not canonical");
    }
}
