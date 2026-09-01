use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeSet;

mod annotations;
mod counters;
mod suffixes;
#[cfg(test)]
mod tests;
mod types;
mod validation;

pub use types::*;

use crate::binary::{
    ByteSlice, align, assert_zero, checked_range, checked_table_end, crc32, magic, u16_at, u32_at,
};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;

const MAGIC: &[u8; 8] = b"IANSUP01";
const VERSION: u16 = 2;
const HEADER_BYTES: usize = 224;
const TABLES: usize = 16;
const NONE: u32 = u32::MAX;

pub(super) const SUFFIX_KEYS: usize = 0;
pub(super) const SUFFIX_VALUES: usize = 1;
pub(super) const SUFFIX_FORMS: usize = 2;
pub(super) const SUFFIX_CONJUGATIONS: usize = 3;
pub(super) const SUFFIX_CLASSES: usize = 4;
pub(super) const COUNTER_KEYS: usize = 5;
pub(super) const COUNTER_VARIANTS: usize = 6;
pub(super) const DIGIT_OPTIONS: usize = 7;
pub(super) const LIST_MEMBERS: usize = 8;
pub(super) const NUMBER_MEMBERS: usize = 9;
pub(super) const SPLITS: usize = 10;
pub(super) const SPLIT_PARTS: usize = 11;
pub(super) const HINTS: usize = 12;
pub(super) const COLLISIONS: usize = 13;
pub(super) const STRINGS: usize = 14;
pub(super) const STRING_BYTES: usize = 15;

pub(super) const STRIDES: [usize; TABLES] =
    [12, 8, 32, 24, 8, 12, 64, 12, 4, 4, 36, 28, 20, 36, 4, 1];

pub struct AnalyzerSupport {
    pub(super) bytes: ByteSlice,
    pub(super) counts: [usize; TABLES],
    pub(super) offsets: [usize; TABLES],
    generated_rules: usize,
    generated_aliases_count: usize,
    generated_rule_aliases_offset: usize,
    strings: RefCell<Vec<Option<String>>>,
    suffix_lengths: Vec<usize>,
    counter_lengths: Vec<usize>,
}

impl AnalyzerSupport {
    pub(crate) fn open(bytes: ByteSlice) -> Result<Self> {
        if bytes.len() < HEADER_BYTES || !magic(&bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete IANSUP01 header",
            ));
        }
        let version = u16_at(&bytes, 8, ErrorCode::InvalidHeader, "support version")?;
        if version != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported analyzer-support version {version}"),
            ));
        }
        if u16_at(&bytes, 10, ErrorCode::InvalidHeader, "support header size")? as usize
            != HEADER_BYTES
            || u32_at(&bytes, 12, ErrorCode::InvalidHeader, "support total size")? as usize
                != bytes.len()
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "analyzer-support header size is invalid",
            ));
        }
        let mut header = bytes[..HEADER_BYTES].to_vec();
        header[16..20].fill(0);
        if crc32(&header)
            != u32_at(
                &bytes,
                16,
                ErrorCode::InvalidHeader,
                "support header checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "analyzer-support header checksum does not match",
            ));
        }
        if crc32(&bytes[HEADER_BYTES..])
            != u32_at(
                &bytes,
                20,
                ErrorCode::CorruptPayload,
                "support payload checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "analyzer-support payload checksum does not match",
            ));
        }
        assert_zero(
            &bytes,
            164,
            HEADER_BYTES,
            ErrorCode::InvalidHeader,
            "support reserved header",
        )?;

        let mut counts = [0_usize; TABLES];
        let mut offsets = [0_usize; TABLES];
        for index in 0..TABLES {
            counts[index] = u32_at(
                &bytes,
                24 + index * 4,
                ErrorCode::InvalidHeader,
                "support table count",
            )? as usize;
            offsets[index] = u32_at(
                &bytes,
                88 + index * 4,
                ErrorCode::InvalidHeader,
                "support table offset",
            )? as usize;
        }
        let generated_rules = u32_at(
            &bytes,
            152,
            ErrorCode::InvalidHeader,
            "generated rule count",
        )? as usize;
        let generated_aliases_count = u32_at(
            &bytes,
            156,
            ErrorCode::InvalidHeader,
            "generated alias count",
        )? as usize;
        let generated_rule_aliases_offset = u32_at(
            &bytes,
            160,
            ErrorCode::InvalidHeader,
            "generated alias offset",
        )? as usize;

        let mut expected = HEADER_BYTES;
        for index in 0..TABLES {
            if offsets[index] != expected {
                return Err(KernelError::new(
                    ErrorCode::InvalidHeader,
                    format!("analyzer-support table {index} has a non-canonical offset"),
                ));
            }
            let count = if index == STRINGS {
                counts[index].checked_add(1).ok_or_else(|| {
                    KernelError::new(
                        ErrorCode::InvalidHeader,
                        "support string-directory count overflows",
                    )
                })?
            } else {
                counts[index]
            };
            expected = checked_table_end(
                offsets[index],
                count,
                STRIDES[index],
                bytes.len(),
                ErrorCode::InvalidHeader,
                "analyzer-support table",
            )?;
        }
        let aliases_at = align(expected, 8)?;
        if generated_rule_aliases_offset != aliases_at {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "generated rule aliases have a non-canonical offset",
            ));
        }
        assert_zero(
            &bytes,
            expected,
            aliases_at,
            ErrorCode::CorruptPayload,
            "support string-data padding",
        )?;
        expected = checked_table_end(
            aliases_at,
            generated_rules,
            2,
            bytes.len(),
            ErrorCode::InvalidHeader,
            "generated rule aliases",
        )?;
        let total = align(expected, 8)?;
        if total != bytes.len() {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "analyzer-support trailing length is not canonical",
            ));
        }
        assert_zero(
            &bytes,
            expected,
            total,
            ErrorCode::CorruptPayload,
            "support trailing padding",
        )?;

        let mut previous = 0_u32;
        for index in 0..=counts[STRINGS] {
            let current = u32_at(
                &bytes,
                offsets[STRINGS] + index * 4,
                ErrorCode::CorruptPayload,
                "support string offset",
            )?;
            if current < previous || current as usize > counts[STRING_BYTES] {
                return Err(KernelError::new(
                    ErrorCode::CorruptPayload,
                    "support string directory is not monotonic",
                ));
            }
            previous = current;
        }
        if previous as usize != counts[STRING_BYTES] {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "support string directory does not cover its pool",
            ));
        }

        let mut support = Self {
            bytes,
            counts,
            offsets,
            generated_rules,
            generated_aliases_count,
            generated_rule_aliases_offset,
            strings: RefCell::new((0..counts[STRINGS]).map(|_| None).collect()),
            suffix_lengths: Vec::new(),
            counter_lengths: Vec::new(),
        };
        support.validate_payload()?;
        support.suffix_lengths = support.key_lengths(SUFFIX_KEYS)?;
        support.counter_lengths = support.key_lengths(COUNTER_KEYS)?;
        Ok(support)
    }

    #[cfg(test)]
    pub fn stats(&self) -> SupportStats {
        SupportStats {
            byte_length: self.bytes.len(),
            suffix_keys: self.counts[SUFFIX_KEYS],
            suffix_values: self.counts[SUFFIX_VALUES],
            suffix_forms: self.counts[SUFFIX_FORMS],
            suffix_conjugations: self.counts[SUFFIX_CONJUGATIONS],
            suffix_classes: self.counts[SUFFIX_CLASSES],
            counter_keys: self.counts[COUNTER_KEYS],
            counter_variants: self.counts[COUNTER_VARIANTS],
            digit_options: self.counts[DIGIT_OPTIONS],
            list_members: self.counts[LIST_MEMBERS],
            number_members: self.counts[NUMBER_MEMBERS],
            splits: self.counts[SPLITS],
            split_parts: self.counts[SPLIT_PARTS],
            hints: self.counts[HINTS],
            collisions: self.counts[COLLISIONS],
            generated_rules: self.generated_rules,
            generated_aliases: self.generated_aliases_count,
            strings: self.counts[STRINGS],
            string_bytes: self.counts[STRING_BYTES],
        }
    }

    pub fn generated_aliases(&self, rule_ids: &[u32]) -> Result<Vec<u16>> {
        if rule_ids.len() != 1 && rule_ids.len() != 2 {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "generated lookup requires one or two rules",
            ));
        }
        rule_ids
            .iter()
            .map(|id| {
                self.assert_index(*id as usize, self.generated_rules, "generated rule")?;
                u16_at(
                    &self.bytes,
                    self.generated_rule_aliases_offset + *id as usize * 2,
                    ErrorCode::CorruptPayload,
                    "generated rule alias",
                )
            })
            .collect()
    }

    pub(super) fn count(&self, table: usize) -> usize {
        self.counts[table]
    }

    pub(super) fn record(&self, table: usize, index: usize, label: &str) -> Result<usize> {
        self.assert_index(index, self.counts[table], label)?;
        checked_table_end(
            self.offsets[table],
            index,
            STRIDES[table],
            self.bytes.len(),
            ErrorCode::CorruptPayload,
            label,
        )
    }

    pub(super) fn assert_index(&self, index: usize, count: usize, label: &str) -> Result<()> {
        if index >= count {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                format!("{label} index {index} is out of range"),
            ));
        }
        Ok(())
    }

    pub(super) fn span(
        &self,
        first: usize,
        count: usize,
        total: usize,
        label: &str,
    ) -> Result<std::ops::Range<usize>> {
        let end = first.checked_add(count).ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptPayload, format!("{label} span overflows"))
        })?;
        if end > total {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                format!("{label} span is out of range"),
            ));
        }
        Ok(first..end)
    }

    pub(super) fn string(&self, id: usize) -> Result<String> {
        self.assert_index(id, self.counts[STRINGS], "support string")?;
        if let Some(value) = self.strings.borrow()[id].as_ref() {
            return Ok(value.clone());
        }
        let value = self.string_slice(id)?.to_owned();
        self.strings.borrow_mut()[id] = Some(value.clone());
        Ok(value)
    }

    pub(super) fn string_slice(&self, id: usize) -> Result<&str> {
        self.assert_index(id, self.counts[STRINGS], "support string")?;
        let start = u32_at(
            &self.bytes,
            self.offsets[STRINGS] + id * 4,
            ErrorCode::CorruptPayload,
            "support string start",
        )? as usize;
        let end = u32_at(
            &self.bytes,
            self.offsets[STRINGS] + (id + 1) * 4,
            ErrorCode::CorruptPayload,
            "support string end",
        )? as usize;
        let bytes = checked_range(
            &self.bytes,
            self.offsets[STRING_BYTES] + start,
            end.checked_sub(start).ok_or_else(|| {
                KernelError::new(
                    ErrorCode::CorruptPayload,
                    "support string range is reversed",
                )
            })?,
            ErrorCode::CorruptPayload,
            "support string",
        )?;
        std::str::from_utf8(bytes).map_err(|_| {
            KernelError::new(
                ErrorCode::CorruptPayload,
                format!("support string {id} is not valid UTF-8"),
            )
        })
    }

    pub(super) fn string_id(&self, at: usize, label: &str) -> Result<usize> {
        let id = self.u32(at, label)? as usize;
        if id >= self.counts[STRINGS] {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                format!("{label} index {id} is out of range"),
            ));
        }
        Ok(id)
    }

    pub(super) fn optional_string(&self, id: u32) -> Result<Option<String>> {
        if id == NONE {
            Ok(None)
        } else {
            self.string(id as usize).map(Some)
        }
    }

    pub(super) fn string_list(&self, first: usize, count: usize) -> Result<Vec<String>> {
        self.span(
            first,
            count,
            self.counts[LIST_MEMBERS],
            "support string list",
        )?
        .map(|index| {
            let at = self.record(LIST_MEMBERS, index, "support string-list member")?;
            self.string(self.string_id(at, "support string-list value")?)
        })
        .collect()
    }

    pub(super) fn number_list(&self, first: usize, count: usize) -> Result<Vec<u32>> {
        self.span(
            first,
            count,
            self.counts[NUMBER_MEMBERS],
            "support number list",
        )?
        .map(|index| {
            let at = self.record(NUMBER_MEMBERS, index, "support number-list member")?;
            self.u32(at, "support number-list value")
        })
        .collect()
    }

    pub(super) fn find_string_key(&self, text: &[u16], table: usize) -> Result<Option<usize>> {
        let mut low = 0;
        let mut high = self.counts[table];
        while low < high {
            let middle = low + (high - low) / 2;
            let at = self.record(table, middle, "support string key")?;
            let id = self.string_id(at, "support string key")?;
            if self.compare_string_to_units(id, text)? == Ordering::Less {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        if low < self.counts[table] {
            let at = self.record(table, low, "support string key")?;
            let id = self.string_id(at, "support string key")?;
            if self.compare_string_to_units(id, text)? == Ordering::Equal {
                return Ok(Some(low));
            }
        }
        Ok(None)
    }

    pub(super) fn compare_strings(&self, left: usize, right: usize) -> Result<Ordering> {
        Ok(self
            .string_slice(left)?
            .encode_utf16()
            .cmp(self.string_slice(right)?.encode_utf16()))
    }

    pub(super) fn compare_string_to_units(&self, id: usize, value: &[u16]) -> Result<Ordering> {
        Ok(self
            .string_slice(id)?
            .encode_utf16()
            .cmp(value.iter().copied()))
    }

    pub(super) fn u8(&self, at: usize, label: &str) -> Result<u8> {
        self.bytes.get(at).copied().ok_or_else(|| {
            KernelError::new(ErrorCode::CorruptPayload, format!("truncated {label}"))
        })
    }

    pub(super) fn u16(&self, at: usize, label: &str) -> Result<u16> {
        u16_at(&self.bytes, at, ErrorCode::CorruptPayload, label)
    }

    pub(super) fn i16(&self, at: usize, label: &str) -> Result<i16> {
        Ok(self.u16(at, label)? as i16)
    }

    pub(super) fn u32(&self, at: usize, label: &str) -> Result<u32> {
        u32_at(&self.bytes, at, ErrorCode::CorruptPayload, label)
    }

    #[cfg(test)]
    pub(super) fn i32(&self, at: usize, label: &str) -> Result<i32> {
        Ok(self.u32(at, label)? as i32)
    }

    pub(super) fn route(&self, code: u8) -> Result<Route> {
        match code {
            0 => Ok(Route::Kana),
            1 => Ok(Route::Kanji),
            _ => Err(KernelError::new(
                ErrorCode::CorruptPayload,
                format!("invalid support route {code}"),
            )),
        }
    }

    pub(super) fn tri(&self, code: u8) -> Result<Option<bool>> {
        match code {
            0 => Ok(Some(false)),
            1 => Ok(Some(true)),
            2 => Ok(None),
            _ => Err(KernelError::new(
                ErrorCode::CorruptPayload,
                format!("invalid support tri-state {code}"),
            )),
        }
    }

    fn key_lengths(&self, table: usize) -> Result<Vec<usize>> {
        let mut lengths = BTreeSet::new();
        for index in 0..self.counts[table] {
            let at = self.record(table, index, "support key")?;
            let id = self.string_id(at, "support key string")?;
            lengths.insert(self.string_slice(id)?.encode_utf16().count());
        }
        Ok(lengths.into_iter().rev().collect())
    }
}
