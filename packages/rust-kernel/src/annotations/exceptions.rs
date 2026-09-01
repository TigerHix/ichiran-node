use std::cmp::Ordering;

use crate::binary::{checked_range, checked_table_end, u16_at, u32_at};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;

use super::{ALIAS_BITS, ALIAS_MAX, KEY_BITS, KEY_MASK};

const ENTRY_BYTES: usize = 16;
const LOCATOR_BYTES: usize = 8;

#[derive(Debug)]
pub(super) struct ExceptionSpan {
    route: Route,
    surface: String,
    first: usize,
    count: usize,
}

pub(super) enum ExceptionLookup {
    NotExceptional,
    Exceptional(Option<u8>),
}

#[allow(clippy::too_many_arguments)]
pub(super) fn validate_exceptions(
    bytes: &[u8],
    entries_offset: usize,
    locators_offset: usize,
    strings_offset: usize,
    surfaces: usize,
    locators: usize,
    classes: usize,
    maximum_rank: usize,
    string_bytes: usize,
) -> Result<Vec<ExceptionSpan>> {
    let mut output = Vec::with_capacity(surfaces);
    let mut locator_total = 0_usize;
    let mut class_total = 0_usize;
    let mut string_total = 0_usize;
    let mut actual_maximum_rank = 0_usize;
    let mut previous_route = None;
    let mut previous_surface = Vec::new();

    for exception in 0..surfaces {
        let at = checked_table_end(
            entries_offset,
            exception,
            ENTRY_BYTES,
            bytes.len(),
            ErrorCode::CorruptIndex,
            "lookup exception entry",
        )?;
        let surface_offset = u32_at(
            bytes,
            at,
            ErrorCode::CorruptIndex,
            "lookup exception surface offset",
        )? as usize;
        let first = u32_at(
            bytes,
            at + 4,
            ErrorCode::CorruptIndex,
            "lookup exception first locator",
        )? as usize;
        let surface_bytes = u16_at(
            bytes,
            at + 8,
            ErrorCode::CorruptIndex,
            "lookup exception surface length",
        )? as usize;
        let count = u16_at(
            bytes,
            at + 10,
            ErrorCode::CorruptIndex,
            "lookup exception locator count",
        )? as usize;
        let route_code = bytes[at + 12];
        let entry_maximum_rank = bytes[at + 13] as usize;
        let reserved = u16_at(
            bytes,
            at + 14,
            ErrorCode::CorruptIndex,
            "lookup exception reserved field",
        )?;
        let surface_end = surface_offset.checked_add(surface_bytes).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception surface span overflows",
            )
        })?;
        let locator_end = first.checked_add(count).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception locator span overflows",
            )
        })?;
        if surface_bytes == 0
            || count == 0
            || route_code > 1
            || entry_maximum_rank > 0x3f
            || reserved != 0
            || surface_offset != string_total
            || first != locator_total
            || surface_end > string_bytes
            || locator_end > locators
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception span is invalid",
            ));
        }
        let encoded_at = strings_offset.checked_add(surface_offset).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception string offset overflows",
            )
        })?;
        let encoded = checked_range(
            bytes,
            encoded_at,
            surface_bytes,
            ErrorCode::CorruptIndex,
            "lookup exception surface",
        )?;
        if previous_route.is_some_and(|route| {
            route > route_code
                || (route == route_code
                    && previous_surface.as_slice().cmp(encoded) != Ordering::Less)
        }) {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exceptions are not canonical",
            ));
        }
        let surface = std::str::from_utf8(encoded).map_err(|_| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception surface is not valid UTF-8",
            )
        })?;
        let route = if route_code == 0 {
            Route::Kana
        } else {
            Route::Kanji
        };

        let distinct_ranks =
            validate_locators(bytes, locators_offset, first, count, entry_maximum_rank)?;
        class_total = class_total.checked_add(distinct_ranks).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception class total overflows",
            )
        })?;
        locator_total = locator_end;
        string_total = surface_end;
        actual_maximum_rank = actual_maximum_rank.max(entry_maximum_rank);
        previous_route = Some(route_code);
        previous_surface.clear();
        previous_surface.extend_from_slice(encoded);
        output.push(ExceptionSpan {
            route,
            surface: surface.to_owned(),
            first,
            count,
        });
    }
    if locator_total != locators
        || class_total != classes
        || string_total != string_bytes
        || actual_maximum_rank != maximum_rank
    {
        return Err(KernelError::new(
            ErrorCode::CorruptIndex,
            "lookup exception totals disagree with the header",
        ));
    }
    Ok(output)
}

fn validate_locators(
    bytes: &[u8],
    offset: usize,
    first: usize,
    count: usize,
    maximum_rank: usize,
) -> Result<usize> {
    let mut ranks = [false; 64];
    let mut previous = None;
    for locator in 0..count {
        let index = first.checked_add(locator).ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception locator index overflows",
            )
        })?;
        let at = checked_table_end(
            offset,
            index,
            LOCATOR_BYTES,
            bytes.len(),
            ErrorCode::CorruptIndex,
            "lookup exception locator",
        )?;
        let root = u32_at(bytes, at, ErrorCode::CorruptIndex, "lookup exception root")?;
        let packed = u32_at(
            bytes,
            at + 4,
            ErrorCode::CorruptIndex,
            "lookup exception packed locator",
        )?;
        let key = packed & KEY_MASK;
        let rank = ((packed >> KEY_BITS) & 0x3f) as usize;
        let first_alias = key >> ALIAS_BITS;
        let second_code = key & ((1 << ALIAS_BITS) - 1);
        if root == 0
            || packed >> 28 != 0
            || (key != KEY_MASK && (first_alias > ALIAS_MAX || second_code > ALIAS_MAX + 1))
            || previous.is_some_and(|(prior_root, prior_key)| {
                root < prior_root || (root == prior_root && key <= prior_key)
            })
        {
            return Err(KernelError::new(
                ErrorCode::CorruptIndex,
                "lookup exception locator is invalid",
            ));
        }
        previous = Some((root, key));
        ranks[rank] = true;
    }
    let distinct = ranks.iter().filter(|rank| **rank).count();
    if distinct < 2
        || distinct != maximum_rank + 1
        || ranks[..=maximum_rank].iter().any(|rank| !rank)
    {
        return Err(KernelError::new(
            ErrorCode::CorruptIndex,
            "lookup exception ranks are not dense",
        ));
    }
    Ok(distinct)
}

pub(super) fn lookup(
    spans: &[ExceptionSpan],
    bytes: &[u8],
    locators_offset: usize,
    route: Route,
    surface: &str,
    root: u32,
    key: u32,
) -> Result<ExceptionLookup> {
    let wanted_route = route.code();
    let Ok(index) = spans.binary_search_by(|span| {
        span.route
            .code()
            .cmp(&wanted_route)
            .then_with(|| span.surface.as_bytes().cmp(surface.as_bytes()))
    }) else {
        return Ok(ExceptionLookup::NotExceptional);
    };
    let span = &spans[index];
    let mut low = 0;
    let mut high = span.count;
    while low < high {
        let middle = low + (high - low) / 2;
        let at = locator_at(bytes, locators_offset, span, middle)?;
        let found_root = u32_at(bytes, at, ErrorCode::CorruptIndex, "lookup exception root")?;
        let found_key = u32_at(
            bytes,
            at + 4,
            ErrorCode::CorruptIndex,
            "lookup exception locator",
        )? & KEY_MASK;
        if found_root < root || (found_root == root && found_key < key) {
            low = middle + 1;
        } else {
            high = middle;
        }
    }
    if low >= span.count {
        return Ok(ExceptionLookup::Exceptional(None));
    }
    let at = locator_at(bytes, locators_offset, span, low)?;
    let found_root = u32_at(bytes, at, ErrorCode::CorruptIndex, "lookup exception root")?;
    let packed = u32_at(
        bytes,
        at + 4,
        ErrorCode::CorruptIndex,
        "lookup exception locator",
    )?;
    if found_root != root || packed & KEY_MASK != key {
        return Ok(ExceptionLookup::Exceptional(None));
    }
    Ok(ExceptionLookup::Exceptional(Some(
        ((packed >> KEY_BITS) & 0x3f) as u8,
    )))
}

fn locator_at(
    bytes: &[u8],
    locators_offset: usize,
    span: &ExceptionSpan,
    relative: usize,
) -> Result<usize> {
    let index = span.first.checked_add(relative).ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptIndex,
            "lookup exception locator index overflows",
        )
    })?;
    checked_table_end(
        locators_offset,
        index,
        LOCATOR_BYTES,
        bytes.len(),
        ErrorCode::CorruptIndex,
        "lookup exception locator",
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn put_u16(bytes: &mut [u8], at: usize, value: u16) {
        bytes[at..at + 2].copy_from_slice(&value.to_le_bytes());
    }

    fn put_u32(bytes: &mut [u8], at: usize, value: u32) {
        bytes[at..at + 4].copy_from_slice(&value.to_le_bytes());
    }

    fn fixture() -> Vec<u8> {
        let mut bytes = vec![0; 16 + 16 + 6];
        put_u32(&mut bytes, 0, 0);
        put_u32(&mut bytes, 4, 0);
        put_u16(&mut bytes, 8, 6);
        put_u16(&mut bytes, 10, 2);
        bytes[12] = 1;
        bytes[13] = 1;
        put_u32(&mut bytes, 16, 20);
        put_u32(&mut bytes, 20, KEY_MASK);
        put_u32(&mut bytes, 24, 30);
        put_u32(&mut bytes, 28, 1 << KEY_BITS);
        bytes[32..].copy_from_slice("例外".as_bytes());
        bytes
    }

    #[test]
    fn exceptional_surface_owns_missing_locator() {
        let bytes = fixture();
        let spans = validate_exceptions(&bytes, 0, 16, 32, 1, 2, 2, 1, 6).unwrap();
        assert!(matches!(
            lookup(&spans, &bytes, 16, Route::Kanji, "例外", 20, KEY_MASK).unwrap(),
            ExceptionLookup::Exceptional(Some(0))
        ));
        assert!(matches!(
            lookup(&spans, &bytes, 16, Route::Kanji, "例外", 99, KEY_MASK).unwrap(),
            ExceptionLookup::Exceptional(None)
        ));
        assert!(matches!(
            lookup(&spans, &bytes, 16, Route::Kana, "例外", 99, KEY_MASK).unwrap(),
            ExceptionLookup::NotExceptional
        ));
    }

    #[test]
    fn rejects_non_dense_ranks() {
        let mut bytes = fixture();
        put_u32(&mut bytes, 28, 2 << KEY_BITS);
        let error = validate_exceptions(&bytes, 0, 16, 32, 1, 2, 3, 2, 6).unwrap_err();
        assert_eq!(error.message, "lookup exception ranks are not dense");
    }

    #[test]
    fn rejects_invalid_utf8() {
        let mut bytes = fixture();
        bytes[32] = 0xff;
        let error = validate_exceptions(&bytes, 0, 16, 32, 1, 2, 2, 1, 6).unwrap_err();
        assert_eq!(error.message, "lookup exception surface is not valid UTF-8");
    }
}
