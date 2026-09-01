use std::sync::Arc;

use crate::binary::{ByteSlice, crc32};
use crate::error::ErrorCode;

use super::{
    AnalyzerAnnotations, BLOCK_BYTES, GENERATED_RECORD_BYTES, GENERATED_ROOT_BYTES, HEADER_BYTES,
    LOOKUP_ORDER_BYTES, MAGIC, VERSION, checked_decoded_records,
};

fn put_u16(bytes: &mut [u8], offset: usize, value: u16) {
    bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
}

fn put_u32(bytes: &mut [u8], offset: usize, value: u32) {
    bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

fn empty_section() -> Vec<u8> {
    let mut bytes = vec![0; HEADER_BYTES];
    bytes[..MAGIC.len()].copy_from_slice(MAGIC);
    put_u16(&mut bytes, 8, VERSION);
    put_u16(&mut bytes, 10, HEADER_BYTES as u16);
    put_u32(&mut bytes, 12, HEADER_BYTES as u32);
    put_u32(&mut bytes, 36, BLOCK_BYTES as u32);
    for offset in [40, 44, 84, 88, 92, 96, 168, 172, 176] {
        put_u32(&mut bytes, offset, HEADER_BYTES as u32);
    }
    put_u32(&mut bytes, 72, BLOCK_BYTES as u32);
    put_u32(&mut bytes, 76, GENERATED_ROOT_BYTES as u32);
    put_u32(&mut bytes, 80, GENERATED_RECORD_BYTES as u32);
    put_u32(&mut bytes, 120, 256 * 1024);
    put_u32(&mut bytes, 140, LOOKUP_ORDER_BYTES as u32);
    put_u32(&mut bytes, 160, 16);
    put_u32(&mut bytes, 164, 8);
    let checksum = crc32(&bytes);
    put_u32(&mut bytes, 16, checksum);
    bytes
}

fn open(bytes: Vec<u8>) -> crate::error::Result<AnalyzerAnnotations> {
    let storage = Arc::new(bytes);
    let section = ByteSlice::new(Arc::clone(&storage), 0, storage.len()).unwrap();
    AnalyzerAnnotations::open(section)
}

#[test]
fn rejects_generated_block_count_that_overflows_wasm32_table_math() {
    let mut bytes = vec![0; HEADER_BYTES];
    bytes[..MAGIC.len()].copy_from_slice(MAGIC);
    put_u16(&mut bytes, 8, VERSION);
    put_u16(&mut bytes, 10, HEADER_BYTES as u16);
    put_u32(&mut bytes, 12, HEADER_BYTES as u32);
    put_u32(&mut bytes, 36, BLOCK_BYTES as u32);
    put_u32(&mut bytes, 40, HEADER_BYTES as u32);
    put_u32(&mut bytes, 44, HEADER_BYTES as u32);
    put_u32(&mut bytes, 52, u32::MAX);
    put_u32(&mut bytes, 72, BLOCK_BYTES as u32);
    put_u32(&mut bytes, 76, GENERATED_ROOT_BYTES as u32);
    put_u32(&mut bytes, 80, GENERATED_RECORD_BYTES as u32);
    for offset in [84, 88, 92, 96, 168, 172, 176] {
        put_u32(&mut bytes, offset, HEADER_BYTES as u32);
    }
    put_u32(&mut bytes, 120, 256 * 1024);
    put_u32(&mut bytes, 140, LOOKUP_ORDER_BYTES as u32);
    put_u32(&mut bytes, 160, 16);
    put_u32(&mut bytes, 164, 8);
    let checksum = crc32(&bytes);
    put_u32(&mut bytes, 16, checksum);

    let storage = Arc::new(bytes);
    let section = ByteSlice::new(Arc::clone(&storage), 0, storage.len()).unwrap();
    let error = AnalyzerAnnotations::open(section)
        .err()
        .expect("oversized generated block table was accepted");

    assert_eq!(error.code, ErrorCode::InvalidHeader);
    assert!(error.message.contains("generated block table"));
}

#[test]
fn rejects_decoded_record_total_as_soon_as_it_exceeds_the_header() {
    let error = checked_decoded_records(7, 2, 8).unwrap_err();

    assert_eq!(error.code, ErrorCode::CorruptBlock);
    assert_eq!(
        error.message,
        "generated decoded record total exceeds the header"
    );
}

#[test]
fn accepts_a_canonical_empty_section() {
    open(empty_section()).expect("canonical empty annotation section");
}

#[test]
fn rejects_nonzero_lookup_totals_without_records() {
    let mut bytes = empty_section();
    put_u32(&mut bytes, 132, 1);
    put_u32(&mut bytes, 16, 0);
    let checksum = crc32(&bytes);
    put_u32(&mut bytes, 16, checksum);
    assert_eq!(open(bytes).err().unwrap().code, ErrorCode::InvalidHeader);
}

#[test]
fn rejects_exception_totals_without_surfaces() {
    let mut bytes = empty_section();
    put_u32(&mut bytes, 152, 1);
    put_u32(&mut bytes, 16, 0);
    let checksum = crc32(&bytes);
    put_u32(&mut bytes, 16, checksum);
    assert_eq!(open(bytes).err().unwrap().code, ErrorCode::InvalidHeader);
}
