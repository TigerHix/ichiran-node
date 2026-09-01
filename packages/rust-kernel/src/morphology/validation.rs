use crate::binary::{align, checked_table_end};
use crate::error::{ErrorCode, KernelError, Result};

use super::{
    HEADER_BYTES, Header, Morphology, PATCH_BUCKET_BYTES, PATCH_BYTES, POS_BYTES, ROOT_FORM_BYTES,
    ROOT_GROUP_BYTES, ROOT_KEY_BYTES, ROOT_RECORD_BYTES, RULE_BYTES, Route, SUFFIX_BYTES,
    TEMPLATE_BYTES, TOMBSTONE_BYTES, corrupt, tri,
};

pub(super) fn covered_end(first: usize, count: usize, limit: usize, label: &str) -> Result<usize> {
    let end = first
        .checked_add(count)
        .ok_or_else(|| corrupt(format!("{label} range overflows")))?;
    if end > limit {
        return Err(corrupt(format!("{label} range exceeds its table")));
    }
    Ok(end)
}

pub(super) fn strictly_after<T: Ord>(
    previous: &mut Option<T>,
    value: T,
    message: &'static str,
) -> Result<()> {
    if previous.as_ref().is_some_and(|prior| prior >= &value) {
        return Err(corrupt(message));
    }
    *previous = Some(value);
    Ok(())
}

pub(super) fn validate_layout(bytes: &[u8], h: Header) -> Result<()> {
    let mut expected = HEADER_BYTES;
    let string_directory_count = string_directory_count(h.strings)?;
    let tables = [
        (h.pos_offset, h.positions, POS_BYTES, "morphology POS table"),
        (h.rule_offset, h.rules, RULE_BYTES, "morphology rule table"),
        (
            h.suffix_offset,
            h.suffixes,
            SUFFIX_BYTES,
            "morphology suffix table",
        ),
        (
            h.template_offset,
            h.templates,
            TEMPLATE_BYTES,
            "morphology template table",
        ),
        (
            h.root_key_offset,
            h.root_keys,
            ROOT_KEY_BYTES,
            "morphology root-key table",
        ),
        (
            h.root_record_offset,
            h.root_records,
            ROOT_RECORD_BYTES,
            "morphology root-record table",
        ),
        (
            h.root_hash_offset,
            h.root_hash_slots,
            4,
            "morphology root-hash table",
        ),
        (
            h.root_group_offset,
            h.root_groups,
            ROOT_GROUP_BYTES,
            "morphology root-group table",
        ),
        (
            h.root_form_offset,
            h.root_forms,
            ROOT_FORM_BYTES,
            "morphology root-form table",
        ),
        (
            h.patch_bucket_offset,
            h.patch_buckets,
            PATCH_BUCKET_BYTES,
            "morphology patch-bucket table",
        ),
        (
            h.patch_offset,
            h.patches,
            PATCH_BYTES,
            "morphology patch table",
        ),
        (
            h.tombstone_offset,
            h.tombstones,
            TOMBSTONE_BYTES,
            "morphology tombstone table",
        ),
        (
            h.string_dir_offset,
            string_directory_count,
            4,
            "morphology string directory",
        ),
    ];
    for (offset, count, stride, label) in tables {
        if offset != expected {
            return Err(KernelError::new(
                ErrorCode::InvalidHeader,
                format!("{label} has a non-canonical offset"),
            ));
        }
        expected = checked_table_end(
            offset,
            count,
            stride,
            bytes.len(),
            ErrorCode::InvalidHeader,
            label,
        )?;
    }
    expected = align(expected, 2)?;
    if h.string_pool_offset != expected {
        return Err(KernelError::new(
            ErrorCode::InvalidHeader,
            "morphology string pool has a non-canonical offset",
        ));
    }
    expected = checked_table_end(
        expected,
        h.string_code_units,
        2,
        bytes.len(),
        ErrorCode::InvalidHeader,
        "morphology string pool",
    )?;
    if align(expected, 4)? != h.byte_length || bytes[expected..].iter().any(|value| *value != 0) {
        return Err(KernelError::new(
            ErrorCode::InvalidHeader,
            "morphology trailing bytes are invalid",
        ));
    }
    Ok(())
}

fn string_directory_count(strings: usize) -> Result<usize> {
    strings.checked_add(1).ok_or_else(|| {
        KernelError::new(
            ErrorCode::InvalidHeader,
            "morphology string-directory count overflows",
        )
    })
}

impl Morphology {
    pub(super) fn validate_strings(&self) -> Result<()> {
        let mut previous = 0_u32;
        for index in 0..=self.header.strings {
            let current = self.u32(self.header.string_dir_offset + index * 4)?;
            if current < previous || current as usize > self.header.string_code_units {
                return Err(corrupt("morphology string directory is not monotonic"));
            }
            previous = current;
        }
        if previous as usize != self.header.string_code_units {
            return Err(corrupt(
                "morphology string directory does not cover its pool",
            ));
        }
        Ok(())
    }

    pub(super) fn validate_records(&self) -> Result<()> {
        let h = self.header;
        let mut hash_entries = 0;
        for slot in 0..h.root_hash_slots {
            let entry = self.u32(h.root_hash_offset + slot * 4)? as usize;
            if entry > h.root_keys {
                return Err(corrupt("morphology root hash references a missing key"));
            }
            hash_entries += usize::from(entry != 0);
        }
        if hash_entries != h.root_keys {
            return Err(corrupt("morphology root hash does not contain every key"));
        }
        let mut next_record = 0;
        for index in 0..h.root_keys {
            let at = h.root_key_offset + index * ROOT_KEY_BYTES;
            self.string_id(self.u32(at)? as usize)?;
            let first = self.u32(at + 4)? as usize;
            let count = self.u32(at + 8)? as usize;
            let end = covered_end(first, count, h.root_records, "morphology root-record")?;
            if first != next_record || count == 0 || self.u16(at + 12)? as usize >= h.positions {
                return Err(corrupt("morphology root keys are not canonical"));
            }
            Route::from_code(self.u8(at + 14)?)?;
            let mut previous_record = None;
            for record in first..end {
                let record_at = h.root_record_offset + record * ROOT_RECORD_BYTES;
                let root_group = self.u32(record_at)?;
                if root_group as usize >= h.root_groups {
                    return Err(corrupt("morphology root record references a missing group"));
                }
                let source_form = self.string_units(self.u32(record_at + 4)? as usize)?;
                let source_reading = self.string_units(self.u32(record_at + 8)? as usize)?;
                strictly_after(
                    &mut previous_record,
                    (
                        root_group,
                        self.u8(record_at + 12)?,
                        self.u8(record_at + 13)?,
                        source_form,
                        source_reading,
                    ),
                    "morphology root records are not strictly sorted",
                )?;
            }
            next_record = end;
        }
        if next_record != h.root_records {
            return Err(corrupt("morphology root records are not covered"));
        }
        let mut next_form = 0;
        for index in 0..h.root_groups {
            let at = h.root_group_offset + index * ROOT_GROUP_BYTES;
            let first = self.u32(at + 4)? as usize;
            let count = self.u32(at + 8)? as usize;
            let end = covered_end(first, count, h.root_forms, "morphology root-form")?;
            if self.u32(at)? == 0 || first != next_form {
                return Err(corrupt("morphology root groups are not canonical"));
            }
            next_form = end;
        }
        if next_form != h.root_forms {
            return Err(corrupt("morphology root forms are not covered"));
        }
        for index in 0..h.root_forms {
            self.string_id(self.u32(h.root_form_offset + index * ROOT_FORM_BYTES)? as usize)?;
        }
        for index in 0..h.rules {
            let at = h.rule_offset + index * RULE_BYTES;
            if self.u16(at)? as usize >= h.positions {
                return Err(corrupt("morphology rule references a missing POS"));
            }
            tri(self.u8(at + 3)? & 3)?;
            tri((self.u8(at + 3)? >> 2) & 3)?;
            self.string_id(self.u32(at + 8)? as usize)?;
            self.string_id(self.u32(at + 12)? as usize)?;
            self.string_id(self.u32(at + 16)? as usize)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_string_directory_count_overflow() {
        let error = string_directory_count(usize::MAX).unwrap_err();

        assert_eq!(error.code, ErrorCode::InvalidHeader);
        assert_eq!(error.message, "morphology string-directory count overflows");
    }
}
