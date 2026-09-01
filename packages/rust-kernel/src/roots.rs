use std::cell::RefCell;

use crate::binary::{
    ByteSlice, align, assert_zero, checked_table_end, crc32, magic, u16_at, u24_at, u32_at, utf8,
};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;

#[cfg(test)]
mod strict_tests;

const MAGIC: &[u8; 8] = b"IROOT002";
const VERSION: u16 = 2;
const HEADER_BYTES: usize = 128;
const SPAN_BYTES: usize = 4;
const FORM_BYTES: usize = 11;
const ENTRY_BYTES: usize = 9;
const RESTRICTION_BYTES: usize = 12;
const POS_SET_BYTES: usize = 6;
const STRING_REF_BIT: u32 = 0x8000_0000;
const STRING_REF_NONE: u32 = 0xffff_ffff;

const ENTRY_PRIMARY_NOKANJI: u8 = 1;
const ENTRY_ARCHIVED: u8 = 1 << 1;
const ENTRY_PREFER_KANA: u8 = 1 << 2;
const ENTRY_PREFER_KANA_ZERO: u8 = 1 << 3;
const ENTRY_FLAGS: u8 =
    ENTRY_PRIMARY_NOKANJI | ENTRY_ARCHIVED | ENTRY_PREFER_KANA | ENTRY_PREFER_KANA_ZERO;

#[derive(Clone, Copy)]
struct Layout {
    spans: usize,
    forms: usize,
    entries: usize,
    restrictions: usize,
    pos_sets: usize,
    pos_members: usize,
    string_offsets: usize,
    string_data: usize,
    string_data_bytes: usize,
}

pub struct RootPayload {
    bytes: ByteSlice,
    pub surface_count: usize,
    pub form_count: usize,
    pub entry_count: usize,
    pub restriction_count: usize,
    string_count: usize,
    pos_set_count: usize,
    pos_member_count: usize,
    layout: Layout,
    strings: RefCell<Vec<Option<String>>>,
}

impl RootPayload {
    pub(crate) fn open(bytes: ByteSlice) -> Result<Self> {
        if bytes.len() < HEADER_BYTES || !magic(&bytes, MAGIC) {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "expected a complete IROOT002 header",
            ));
        }
        let version = u16_at(&bytes, 8, ErrorCode::InvalidHeader, "root version")?;
        if version != VERSION {
            return Err(KernelError::new(
                ErrorCode::UnsupportedVersion,
                format!("unsupported root payload version {version}"),
            ));
        }
        if u16_at(&bytes, 10, ErrorCode::InvalidHeader, "root header size")? as usize
            != HEADER_BYTES
            || u32_at(&bytes, 12, ErrorCode::InvalidHeader, "root flags")? != 0
            || u32_at(&bytes, 28, ErrorCode::InvalidHeader, "root reserved field")? != 0
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root header size or flags are invalid",
            ));
        }
        assert_zero(
            &bytes,
            100,
            HEADER_BYTES,
            ErrorCode::InvalidHeader,
            "root reserved header",
        )?;
        if u32_at(&bytes, 16, ErrorCode::InvalidHeader, "root total size")? as usize != bytes.len()
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root payload byte length does not match",
            ));
        }
        let mut header = bytes[..HEADER_BYTES].to_vec();
        header[20..24].fill(0);
        if crc32(&header) != u32_at(&bytes, 20, ErrorCode::InvalidHeader, "root header checksum")? {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root header checksum does not match",
            ));
        }
        if crc32(&bytes[HEADER_BYTES..])
            != u32_at(
                &bytes,
                24,
                ErrorCode::CorruptPayload,
                "root payload checksum",
            )?
        {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "root payload checksum does not match",
            ));
        }

        let surface_count =
            u32_at(&bytes, 32, ErrorCode::InvalidHeader, "root surface count")? as usize;
        let form_count = u32_at(&bytes, 36, ErrorCode::InvalidHeader, "root form count")? as usize;
        let entry_count =
            u32_at(&bytes, 40, ErrorCode::InvalidHeader, "root entry count")? as usize;
        let restriction_count = u32_at(
            &bytes,
            44,
            ErrorCode::InvalidHeader,
            "root restriction count",
        )? as usize;
        let string_count =
            u32_at(&bytes, 48, ErrorCode::InvalidHeader, "root string count")? as usize;
        let pos_set_count =
            u32_at(&bytes, 52, ErrorCode::InvalidHeader, "root POS-set count")? as usize;
        let pos_member_count = u32_at(
            &bytes,
            56,
            ErrorCode::InvalidHeader,
            "root POS-member count",
        )? as usize;
        if bytes[60] as usize != SPAN_BYTES
            || bytes[61] as usize != FORM_BYTES
            || bytes[62] as usize != ENTRY_BYTES
            || bytes[63] as usize != RESTRICTION_BYTES
        {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root record strides are invalid",
            ));
        }
        let layout = Layout {
            spans: u32_at(&bytes, 64, ErrorCode::InvalidHeader, "root spans offset")? as usize,
            forms: u32_at(&bytes, 68, ErrorCode::InvalidHeader, "root forms offset")? as usize,
            entries: u32_at(&bytes, 72, ErrorCode::InvalidHeader, "root entries offset")? as usize,
            restrictions: u32_at(
                &bytes,
                76,
                ErrorCode::InvalidHeader,
                "root restrictions offset",
            )? as usize,
            pos_sets: u32_at(&bytes, 80, ErrorCode::InvalidHeader, "root POS-set offset")? as usize,
            pos_members: u32_at(
                &bytes,
                84,
                ErrorCode::InvalidHeader,
                "root POS-member offset",
            )? as usize,
            string_offsets: u32_at(&bytes, 88, ErrorCode::InvalidHeader, "root string offsets")?
                as usize,
            string_data: u32_at(
                &bytes,
                92,
                ErrorCode::InvalidHeader,
                "root string data offset",
            )? as usize,
            string_data_bytes: u32_at(
                &bytes,
                96,
                ErrorCode::InvalidHeader,
                "root string data size",
            )? as usize,
        };
        let string_offset_count = string_count.checked_add(1).ok_or_else(|| {
            KernelError::new(
                ErrorCode::InvalidHeader,
                "root string-directory count overflows",
            )
        })?;
        let sections = [
            (layout.spans, surface_count, SPAN_BYTES, "root spans"),
            (layout.forms, form_count, FORM_BYTES, "root forms"),
            (layout.entries, entry_count, ENTRY_BYTES, "root entries"),
            (
                layout.restrictions,
                restriction_count,
                RESTRICTION_BYTES,
                "root restrictions",
            ),
            (
                layout.pos_sets,
                pos_set_count,
                POS_SET_BYTES,
                "root POS sets",
            ),
            (layout.pos_members, pos_member_count, 2, "root POS members"),
            (
                layout.string_offsets,
                string_offset_count,
                4,
                "root string offsets",
            ),
            (
                layout.string_data,
                layout.string_data_bytes,
                1,
                "root string data",
            ),
        ];
        let mut expected = align(HEADER_BYTES, 8)?;
        for (offset, count, stride, label) in sections {
            if offset != expected {
                return Err(KernelError::new(
                    ErrorCode::InvalidHeader,
                    format!("{label} has a non-canonical offset"),
                ));
            }
            let end = checked_table_end(
                offset,
                count,
                stride,
                bytes.len(),
                ErrorCode::InvalidHeader,
                label,
            )?;
            let next = align(end, 8)?;
            assert_zero(&bytes, end, next, ErrorCode::InvalidHeader, label)?;
            expected = next;
        }
        if expected != bytes.len() || string_count == 0 || pos_set_count == 0 {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root payload layout or required empty records are invalid",
            ));
        }

        let mut previous = 0_u32;
        for index in 0..=string_count {
            let current = u32_at(
                &bytes,
                layout.string_offsets + index * 4,
                ErrorCode::InvalidHeader,
                "root string offset",
            )?;
            if current < previous || current as usize > layout.string_data_bytes {
                return Err(KernelError::new(
                    ErrorCode::InvalidHeader,
                    "root string offsets are not monotonic",
                ));
            }
            previous = current;
        }
        if previous as usize != layout.string_data_bytes {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root string directory does not cover its data",
            ));
        }
        let mut next_form = 0_usize;
        for rank in 0..surface_count {
            let at = layout.spans + rank * SPAN_BYTES;
            let first = u24_at(&bytes, at, ErrorCode::InvalidHeader, "root span")? as usize;
            let count = bytes[at + 3] as usize;
            if first != next_form || count == 0 {
                return Err(KernelError::new(
                    ErrorCode::InvalidHeader,
                    "root spans do not form a canonical partition",
                ));
            }
            next_form += count;
        }
        if next_form != form_count {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root spans do not cover forms",
            ));
        }
        let mut next_member = 0_usize;
        for set in 0..pos_set_count {
            let at = layout.pos_sets + set * POS_SET_BYTES;
            let first = u32_at(&bytes, at, ErrorCode::InvalidHeader, "root POS set")? as usize;
            let count = u16_at(&bytes, at + 4, ErrorCode::InvalidHeader, "root POS set")? as usize;
            if first != next_member {
                return Err(KernelError::new(
                    ErrorCode::InvalidHeader,
                    "root POS sets do not form a canonical partition",
                ));
            }
            next_member = checked_pos_member_end(next_member, count, pos_member_count)?;
        }
        if next_member != pos_member_count {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                "root POS sets do not cover members",
            ));
        }
        Ok(Self {
            bytes,
            surface_count,
            form_count,
            entry_count,
            restriction_count,
            string_count,
            pos_set_count,
            pos_member_count,
            layout,
            strings: RefCell::new(vec![None; string_count]),
        })
    }

    pub fn surface_form_start(&self, rank: u32) -> Result<usize> {
        let rank = self.index(rank as usize, self.surface_count, "surface rank")?;
        Ok(u24_at(
            &self.bytes,
            self.layout.spans + rank * SPAN_BYTES,
            ErrorCode::CorruptPayload,
            "root span",
        )? as usize)
    }

    pub fn surface_form_count(&self, rank: u32) -> Result<usize> {
        let rank = self.index(rank as usize, self.surface_count, "surface rank")?;
        Ok(self.bytes[self.layout.spans + rank * SPAN_BYTES + 3] as usize)
    }

    pub fn form_entry_index(&self, form: usize) -> Result<usize> {
        let at = self.form_offset(form)?;
        Ok(u24_at(
            &self.bytes,
            at,
            ErrorCode::CorruptPayload,
            "root form entry",
        )? as usize)
    }

    pub fn form_best_reference(&self, form: usize) -> Result<u32> {
        u32_at(
            &self.bytes,
            self.form_offset(form)? + 3,
            ErrorCode::CorruptPayload,
            "root best reference",
        )
    }

    pub fn form_common_tag_string_id(&self, form: usize) -> Result<u16> {
        u16_at(
            &self.bytes,
            self.form_offset(form)? + 7,
            ErrorCode::CorruptPayload,
            "root common-tag string ID",
        )
    }

    pub fn form_common(&self, form: usize) -> Result<Option<u8>> {
        let value = self.bytes[self.form_offset(form)? + 9] & 0x3f;
        Ok((value != 63).then_some(value))
    }

    pub fn form_route(&self, form: usize) -> Result<Route> {
        Ok(if self.bytes[self.form_offset(form)? + 9] & 0x40 != 0 {
            Route::Kana
        } else {
            Route::Kanji
        })
    }

    pub fn form_conjugatable(&self, form: usize) -> Result<bool> {
        Ok(self.bytes[self.form_offset(form)? + 9] & 0x80 != 0)
    }

    pub fn form_ordinal(&self, form: usize) -> Result<u8> {
        Ok(self.bytes[self.form_offset(form)? + 10] & 0x7f)
    }

    pub fn form_nokanji(&self, form: usize) -> Result<bool> {
        Ok(self.bytes[self.form_offset(form)? + 10] & 0x80 != 0)
    }

    pub fn entry_seq(&self, entry: usize) -> Result<u32> {
        u32_at(
            &self.bytes,
            self.entry_offset(entry)?,
            ErrorCode::CorruptPayload,
            "root entry sequence",
        )
    }

    pub fn entry_n_kanji(&self, entry: usize) -> Result<u8> {
        Ok(self.bytes[self.entry_offset(entry)? + 6])
    }

    pub fn entry_n_kana(&self, entry: usize) -> Result<u8> {
        Ok(self.bytes[self.entry_offset(entry)? + 7])
    }

    pub fn entry_primary_nokanji(&self, entry: usize) -> Result<bool> {
        Ok(self.entry_flags(entry)? & ENTRY_PRIMARY_NOKANJI != 0)
    }

    pub fn entry_archived(&self, entry: usize) -> Result<bool> {
        Ok(self.entry_flags(entry)? & ENTRY_ARCHIVED != 0)
    }

    pub fn entry_prefer_kana(&self, entry: usize) -> Result<bool> {
        Ok(self.entry_flags(entry)? & ENTRY_PREFER_KANA != 0)
    }

    pub fn entry_prefer_kana_zero(&self, entry: usize) -> Result<bool> {
        Ok(self.entry_flags(entry)? & ENTRY_PREFER_KANA_ZERO != 0)
    }

    pub fn entry_positions(&self, entry: usize) -> Result<Vec<String>> {
        let at = self.entry_offset(entry)?;
        let set = u16_at(
            &self.bytes,
            at + 4,
            ErrorCode::CorruptPayload,
            "root POS-set index",
        )? as usize;
        self.index(set, self.pos_set_count, "POS-set index")?;
        let set_at = self.layout.pos_sets + set * POS_SET_BYTES;
        let first = u32_at(
            &self.bytes,
            set_at,
            ErrorCode::CorruptPayload,
            "root POS set",
        )? as usize;
        let count = u16_at(
            &self.bytes,
            set_at + 4,
            ErrorCode::CorruptPayload,
            "root POS set",
        )? as usize;
        let mut positions = Vec::with_capacity(count);
        for offset in 0..count {
            self.index(first + offset, self.pos_member_count, "POS member")?;
            let id = u16_at(
                &self.bytes,
                self.layout.pos_members + (first + offset) * 2,
                ErrorCode::CorruptPayload,
                "POS string ID",
            )? as usize;
            positions.push(self.string(id)?);
        }
        Ok(positions)
    }

    pub fn find_entry_index(&self, seq: u32) -> Result<Option<usize>> {
        let mut low = 0_usize;
        let mut high = self.entry_count;
        while low < high {
            let middle = (low + high) / 2;
            match self.entry_seq(middle)?.cmp(&seq) {
                std::cmp::Ordering::Less => low = middle + 1,
                std::cmp::Ordering::Greater => high = middle,
                std::cmp::Ordering::Equal => return Ok(Some(middle)),
            }
        }
        Ok(None)
    }

    pub fn restriction_entry_index(&self, restriction: usize) -> Result<usize> {
        Ok(u32_at(
            &self.bytes,
            self.restriction_offset(restriction)?,
            ErrorCode::CorruptPayload,
            "root restriction entry",
        )? as usize)
    }

    pub fn restriction_reading_reference(&self, restriction: usize) -> Result<u32> {
        u32_at(
            &self.bytes,
            self.restriction_offset(restriction)? + 4,
            ErrorCode::CorruptPayload,
            "root restriction reading",
        )
    }

    pub fn restriction_written_reference(&self, restriction: usize) -> Result<u32> {
        u32_at(
            &self.bytes,
            self.restriction_offset(restriction)? + 8,
            ErrorCode::CorruptPayload,
            "root restriction written form",
        )
    }

    pub fn restriction_start(&self, entry: usize) -> Result<usize> {
        self.index(entry, self.entry_count, "root entry")?;
        let mut low = 0_usize;
        let mut high = self.restriction_count;
        while low < high {
            let middle = (low + high) / 2;
            if self.restriction_entry_index(middle)? < entry {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        Ok(low)
    }

    pub fn restriction_end(&self, entry: usize) -> Result<usize> {
        self.index(entry, self.entry_count, "root entry")?;
        let mut low = 0_usize;
        let mut high = self.restriction_count;
        while low < high {
            let middle = (low + high) / 2;
            if self.restriction_entry_index(middle)? <= entry {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        Ok(low)
    }

    pub fn surface_reference_is_none(&self, reference: u32) -> bool {
        reference == STRING_REF_NONE
    }

    pub fn surface_reference_is_string(&self, reference: u32) -> bool {
        reference != STRING_REF_NONE && reference & STRING_REF_BIT != 0
    }

    pub fn surface_reference_rank(&self, reference: u32) -> Result<u32> {
        if self.surface_reference_is_none(reference) || self.surface_reference_is_string(reference)
        {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "surface reference is not a direct rank",
            ));
        }
        self.index(
            reference as usize,
            self.surface_count,
            "referenced surface rank",
        )?;
        Ok(reference)
    }

    pub fn surface_reference_string_id(&self, reference: u32) -> Result<usize> {
        if !self.surface_reference_is_string(reference) {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "surface reference is not a string ID",
            ));
        }
        let id = (reference & !STRING_REF_BIT) as usize;
        self.index(id, self.string_count, "referenced string ID")
    }

    pub fn resolve_surface_reference<F>(
        &self,
        reference: u32,
        direct_surface: F,
    ) -> Result<Option<String>>
    where
        F: FnOnce(u32) -> Result<String>,
    {
        if reference == STRING_REF_NONE {
            Ok(None)
        } else if reference & STRING_REF_BIT != 0 {
            Ok(Some(self.string((reference & !STRING_REF_BIT) as usize)?))
        } else {
            self.index(
                reference as usize,
                self.surface_count,
                "referenced surface rank",
            )?;
            direct_surface(reference).map(Some)
        }
    }

    pub fn string(&self, id: usize) -> Result<String> {
        self.index(id, self.string_count, "root string ID")?;
        if let Some(value) = self.strings.borrow()[id].clone() {
            return Ok(value);
        }
        let start = u32_at(
            &self.bytes,
            self.layout.string_offsets + id * 4,
            ErrorCode::CorruptPayload,
            "root string offset",
        )? as usize;
        let end = u32_at(
            &self.bytes,
            self.layout.string_offsets + (id + 1) * 4,
            ErrorCode::CorruptPayload,
            "root string offset",
        )? as usize;
        let value = utf8(
            &self.bytes[self.layout.string_data + start..self.layout.string_data + end],
            ErrorCode::CorruptPayload,
            "root string",
        )?;
        self.strings.borrow_mut()[id] = Some(value.clone());
        Ok(value)
    }

    fn form_offset(&self, form: usize) -> Result<usize> {
        Ok(self.layout.forms + self.index(form, self.form_count, "root form")? * FORM_BYTES)
    }

    fn entry_offset(&self, entry: usize) -> Result<usize> {
        Ok(self.layout.entries + self.index(entry, self.entry_count, "root entry")? * ENTRY_BYTES)
    }

    fn restriction_offset(&self, restriction: usize) -> Result<usize> {
        Ok(self.layout.restrictions
            + self.index(restriction, self.restriction_count, "root restriction")?
                * RESTRICTION_BYTES)
    }

    fn entry_flags(&self, entry: usize) -> Result<u8> {
        let flags = self.bytes[self.entry_offset(entry)? + 8];
        if flags & !ENTRY_FLAGS != 0 {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "root entry has unknown flags",
            ));
        }
        Ok(flags)
    }

    fn index(&self, index: usize, count: usize, label: &str) -> Result<usize> {
        if index >= count {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                format!("{label} {index} is outside [0, {count})"),
            ));
        }
        Ok(index)
    }
}

fn checked_pos_member_end(current: usize, count: usize, declared: usize) -> Result<usize> {
    let end = current.checked_add(count).ok_or_else(|| {
        KernelError::new(
            ErrorCode::InvalidHeader,
            "root POS-set member total overflows",
        )
    })?;
    if end > declared {
        return Err(KernelError::new(
            ErrorCode::InvalidHeader,
            "root POS sets exceed the declared member count",
        ));
    }
    Ok(end)
}
