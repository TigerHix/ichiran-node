use std::ops::Deref;
use std::sync::Arc;

use crate::error::{ErrorCode, KernelError, Result};

#[derive(Clone)]
pub struct ByteSlice {
    bytes: Arc<Vec<u8>>,
    start: usize,
    end: usize,
}

impl ByteSlice {
    pub fn new(bytes: Arc<Vec<u8>>, start: usize, length: usize) -> Result<Self> {
        let end = start
            .checked_add(length)
            .ok_or_else(|| KernelError::new(ErrorCode::OutOfRange, "byte slice end overflows"))?;
        if end > bytes.len() {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "byte slice lies outside its storage",
            ));
        }
        Ok(Self { bytes, start, end })
    }
}

impl Deref for ByteSlice {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.bytes[self.start..self.end]
    }
}

pub fn u16_at(bytes: &[u8], offset: usize, code: ErrorCode, label: &str) -> Result<u16> {
    let end = offset
        .checked_add(2)
        .ok_or_else(|| KernelError::new(code, format!("{label} offset overflows")))?;
    let value = bytes
        .get(offset..end)
        .ok_or_else(|| KernelError::new(code, format!("truncated {label}")))?;
    Ok(u16::from_le_bytes([value[0], value[1]]))
}

pub fn u24_at(bytes: &[u8], offset: usize, code: ErrorCode, label: &str) -> Result<u32> {
    let end = offset
        .checked_add(3)
        .ok_or_else(|| KernelError::new(code, format!("{label} offset overflows")))?;
    let value = bytes
        .get(offset..end)
        .ok_or_else(|| KernelError::new(code, format!("truncated {label}")))?;
    Ok(u32::from(value[0]) | (u32::from(value[1]) << 8) | (u32::from(value[2]) << 16))
}

pub fn u32_at(bytes: &[u8], offset: usize, code: ErrorCode, label: &str) -> Result<u32> {
    let end = offset
        .checked_add(4)
        .ok_or_else(|| KernelError::new(code, format!("{label} offset overflows")))?;
    let value = bytes
        .get(offset..end)
        .ok_or_else(|| KernelError::new(code, format!("truncated {label}")))?;
    Ok(u32::from_le_bytes([value[0], value[1], value[2], value[3]]))
}

pub fn checked_range<'a>(
    bytes: &'a [u8],
    offset: usize,
    length: usize,
    code: ErrorCode,
    label: &str,
) -> Result<&'a [u8]> {
    bytes
        .get(
            offset
                ..offset
                    .checked_add(length)
                    .ok_or_else(|| KernelError::new(code, format!("{label} range overflows")))?,
        )
        .ok_or_else(|| KernelError::new(code, format!("{label} lies outside its container")))
}

pub fn checked_table_end(
    offset: usize,
    count: usize,
    stride: usize,
    total: usize,
    code: ErrorCode,
    label: &str,
) -> Result<usize> {
    let length = count
        .checked_mul(stride)
        .ok_or_else(|| KernelError::new(code, format!("{label} length overflows")))?;
    let end = offset
        .checked_add(length)
        .ok_or_else(|| KernelError::new(code, format!("{label} end overflows")))?;
    if end > total {
        return Err(KernelError::new(
            code,
            format!("{label} lies outside its container"),
        ));
    }
    Ok(end)
}

pub fn assert_zero(
    bytes: &[u8],
    start: usize,
    end: usize,
    code: ErrorCode,
    label: &str,
) -> Result<()> {
    let padding = checked_range(bytes, start, end.saturating_sub(start), code, label)?;
    if let Some(index) = padding.iter().position(|value| *value != 0) {
        return Err(KernelError::new(
            code,
            format!("{label} byte {} is non-zero", start + index),
        ));
    }
    Ok(())
}

pub fn align(value: usize, alignment: usize) -> Result<usize> {
    value
        .checked_add(alignment - 1)
        .map(|sum| sum / alignment * alignment)
        .ok_or_else(|| KernelError::new(ErrorCode::InvalidHeader, "aligned offset overflows"))
}

pub fn crc32(bytes: &[u8]) -> u32 {
    crc32fast::hash(bytes)
}

pub fn magic(bytes: &[u8], expected: &[u8]) -> bool {
    bytes.get(..expected.len()) == Some(expected)
}

pub fn utf8(bytes: &[u8], code: ErrorCode, label: &str) -> Result<String> {
    std::str::from_utf8(bytes)
        .map(str::to_owned)
        .map_err(|_| KernelError::new(code, format!("{label} is not valid UTF-8")))
}
