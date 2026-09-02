use std::sync::Arc;

use crate::binary::ByteSlice;
use crate::error::ErrorCode;

use super::{EDGE_BYTES, HEADER_BYTES, MAGIC, STATE_BYTES, SurfaceIndex, VERSION};

fn put_u16(bytes: &mut [u8], offset: usize, value: u16) {
    bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
}

fn put_u32(bytes: &mut [u8], offset: usize, value: u32) {
    bytes[offset..offset + 4].copy_from_slice(&value.to_le_bytes());
}

#[test]
fn rejects_route_counts_that_overflow_before_overlap_is_removed() {
    let total_bytes = HEADER_BYTES + 2 * STATE_BYTES;
    let mut bytes = vec![0; total_bytes];
    bytes[..MAGIC.len()].copy_from_slice(MAGIC);
    put_u16(&mut bytes, 8, VERSION);
    put_u16(&mut bytes, 10, HEADER_BYTES as u16);
    put_u32(&mut bytes, 16, 1);
    put_u32(&mut bytes, 24, u32::MAX);
    put_u32(&mut bytes, 28, u32::MAX);
    put_u32(&mut bytes, 32, 1);
    put_u32(&mut bytes, 40, u32::MAX);
    put_u32(&mut bytes, 48, HEADER_BYTES as u32);
    put_u32(&mut bytes, 52, total_bytes as u32);
    put_u32(&mut bytes, 56, total_bytes as u32);
    put_u16(&mut bytes, 60, STATE_BYTES as u16);
    put_u16(&mut bytes, 62, EDGE_BYTES as u16);

    let storage = Arc::new(bytes);
    let section = ByteSlice::new(Arc::clone(&storage), 0, storage.len()).unwrap();
    let error = SurfaceIndex::open(section)
        .err()
        .expect("overflowing surface route counts were accepted");

    assert_eq!(error.code, ErrorCode::InvalidHeader);
    assert_eq!(error.message, "surface accepted count overflows");
}
