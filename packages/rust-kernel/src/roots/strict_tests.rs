use std::sync::Arc;

use crate::binary::{ByteSlice, crc32};
use crate::error::ErrorCode;

use super::{
    ENTRY_BYTES, FORM_BYTES, HEADER_BYTES, MAGIC, RESTRICTION_BYTES, RootPayload, SPAN_BYTES,
    VERSION, checked_pos_member_end,
};

fn put_u16(bytes: &mut [u8], offset: usize, value: u16) {
    bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
}

fn put_u32(bytes: &mut [u8], offset: usize, value: u32) {
    bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

#[test]
fn rejects_maximum_portable_string_count_without_wrapping() {
    let mut bytes = vec![0; HEADER_BYTES + 8];
    bytes[..MAGIC.len()].copy_from_slice(MAGIC);
    put_u16(&mut bytes, 8, VERSION);
    put_u16(&mut bytes, 10, HEADER_BYTES as u16);
    let total_bytes = bytes.len() as u32;
    put_u32(&mut bytes, 16, total_bytes);
    put_u32(&mut bytes, 48, u32::MAX);
    put_u32(&mut bytes, 52, 1);
    bytes[60] = SPAN_BYTES as u8;
    bytes[61] = FORM_BYTES as u8;
    bytes[62] = ENTRY_BYTES as u8;
    bytes[63] = RESTRICTION_BYTES as u8;
    for offset in [64, 68, 72, 76, 80] {
        put_u32(&mut bytes, offset, HEADER_BYTES as u32);
    }
    for offset in [84, 88, 92] {
        put_u32(&mut bytes, offset, (HEADER_BYTES + 8) as u32);
    }
    let payload_checksum = crc32(&bytes[HEADER_BYTES..]);
    put_u32(&mut bytes, 24, payload_checksum);
    let header_checksum = crc32(&bytes[..HEADER_BYTES]);
    put_u32(&mut bytes, 20, header_checksum);

    let storage = Arc::new(bytes);
    let section = ByteSlice::new(Arc::clone(&storage), 0, storage.len()).unwrap();
    let error = RootPayload::open(section)
        .err()
        .expect("maximum root string count was accepted");

    assert_eq!(error.code, ErrorCode::InvalidHeader);
    assert!(error.message.contains("root string"));
}

#[test]
fn rejects_pos_member_overcount_and_wasm32_sized_overflow() {
    let overcount = checked_pos_member_end(2, 2, 3).unwrap_err();
    assert_eq!(overcount.code, ErrorCode::InvalidHeader);
    assert_eq!(
        overcount.message,
        "root POS sets exceed the declared member count"
    );

    let overflow = checked_pos_member_end(usize::MAX, 1, usize::MAX).unwrap_err();
    assert_eq!(overflow.code, ErrorCode::InvalidHeader);
    assert_eq!(overflow.message, "root POS-set member total overflows");
}
