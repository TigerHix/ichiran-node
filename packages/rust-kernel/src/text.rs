use crate::error::{ErrorCode, KernelError, Result};

pub fn next_scalar(text: &[u16], offset: usize) -> (u32, usize) {
    let first = u32::from(text[offset]);
    if (0xd800..=0xdbff).contains(&first) && offset + 1 < text.len() {
        let second = u32::from(text[offset + 1]);
        if (0xdc00..=0xdfff).contains(&second) {
            return (0x1_0000 + ((first - 0xd800) << 10) + second - 0xdc00, 2);
        }
    }
    if (0xd800..=0xdfff).contains(&first) {
        (0xfffd, 1)
    } else {
        (first, 1)
    }
}

pub fn scalar_utf8(scalar: u32) -> Vec<u8> {
    let scalar = char::from_u32(scalar).unwrap_or('\u{fffd}');
    let mut buffer = [0_u8; 4];
    scalar.encode_utf8(&mut buffer).as_bytes().to_vec()
}

pub fn utf16(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}

pub fn string(value: &[u16], label: &str) -> Result<String> {
    String::from_utf16(value).map_err(|_| {
        KernelError::new(
            ErrorCode::InvalidInput,
            format!("{label} contains an unpaired UTF-16 surrogate"),
        )
    })
}

pub fn count_kanji(value: &[u16]) -> usize {
    value
        .iter()
        .filter(|code| {
            let code = **code as u32;
            (0x3400..=0x4dbf).contains(&code)
                || (0x4e00..=0x9fff).contains(&code)
                || (0xf900..=0xfaff).contains(&code)
                || code == 0x3005
        })
        .count()
}

pub fn mora_length(value: &[u16]) -> usize {
    crate::characters::mora_length(value)
}
