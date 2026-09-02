use std::cmp::Ordering;

use crate::binary::{assert_zero, u16_at};
use crate::error::{ErrorCode, KernelError, Result};

use super::counters::validate_counter_class;
use super::{
    AnalyzerSupport, COLLISIONS, COUNTER_KEYS, COUNTER_VARIANTS, DIGIT_OPTIONS, HINTS,
    LIST_MEMBERS, NONE, SPLIT_PARTS, SPLITS, STRINGS, SUFFIX_CLASSES, SUFFIX_CONJUGATIONS,
    SUFFIX_FORMS, SUFFIX_KEYS, SUFFIX_VALUES,
};

impl AnalyzerSupport {
    pub(super) fn validate_payload(&self) -> Result<()> {
        self.validate_aliases()?;
        self.validate_list_members()?;
        self.validate_suffixes()?;
        self.validate_counters()?;
        self.validate_annotations()?;
        Ok(())
    }

    fn validate_aliases(&self) -> Result<()> {
        for index in 0..self.generated_rules {
            let alias = u16_at(
                &self.bytes,
                self.generated_rule_aliases_offset + index * 2,
                ErrorCode::CorruptPayload,
                "generated rule alias",
            )? as usize;
            if alias >= self.generated_aliases_count {
                return corrupt("generated rule alias is out of range");
            }
        }
        Ok(())
    }

    fn validate_list_members(&self) -> Result<()> {
        for index in 0..self.count(LIST_MEMBERS) {
            let at = self.record(LIST_MEMBERS, index, "support string-list member")?;
            self.string_id(at, "support string-list value")?;
        }
        Ok(())
    }

    fn validate_suffixes(&self) -> Result<()> {
        let mut next_value = 0;
        let mut prior_key = None;
        for index in 0..self.count(SUFFIX_KEYS) {
            let at = self.record(SUFFIX_KEYS, index, "support suffix key")?;
            let key = self.string_id(at, "support suffix key string")?;
            if let Some(prior) = prior_key
                && self.compare_strings(prior, key)? != Ordering::Less
            {
                return corrupt("support suffix keys are not canonically ordered");
            }
            prior_key = Some(key);
            let first = self.u32(at + 4, "support suffix-value start")? as usize;
            let count = self.u16(at + 8, "support suffix-value count")? as usize;
            if count == 0 || first != next_value {
                return corrupt("support suffix-value spans are not canonical");
            }
            next_value = self
                .span(
                    first,
                    count,
                    self.count(SUFFIX_VALUES),
                    "support suffix values",
                )?
                .end;
            self.zero(at + 10, at + 12, "support suffix-key reserved bytes")?;
        }
        if next_value != self.count(SUFFIX_VALUES) {
            return corrupt("support suffix keys do not cover the value table");
        }
        for index in 0..self.count(SUFFIX_VALUES) {
            let at = self.record(SUFFIX_VALUES, index, "support suffix value")?;
            self.string_id(at, "support suffix keyword")?;
            let form = self.u32(at + 4, "support suffix form")?;
            if form != NONE && form as usize >= self.count(SUFFIX_FORMS) {
                return corrupt("support suffix form is out of range");
            }
        }

        let mut next_conjugation = 0;
        for index in 0..self.count(SUFFIX_FORMS) {
            let at = self.record(SUFFIX_FORMS, index, "support suffix form")?;
            self.string_id(at + 4, "support suffix-form text")?;
            self.validate_optional_string(self.u32(at + 8, "support suffix best kanji")?)?;
            self.string_id(at + 12, "support suffix common tags")?;
            let first = self.u32(at + 16, "support suffix-conjugation start")? as usize;
            let count = self.u16(at + 22, "support suffix-conjugation count")? as usize;
            let flags = self.u8(at + 25, "support suffix-form flags")?;
            if flags & !7 != 0 || (flags & 4 != 0 && count != 0) || first != next_conjugation {
                return corrupt("support suffix-form flags or conjugation span is invalid");
            }
            next_conjugation = self
                .span(
                    first,
                    count,
                    self.count(SUFFIX_CONJUGATIONS),
                    "support suffix conjugations",
                )?
                .end;
            self.zero(at + 26, at + 32, "support suffix-form reserved bytes")?;
        }
        if next_conjugation != self.count(SUFFIX_CONJUGATIONS) {
            return corrupt("support suffix forms do not cover the conjugation table");
        }
        for index in 0..self.count(SUFFIX_CONJUGATIONS) {
            let at = self.record(SUFFIX_CONJUGATIONS, index, "support suffix conjugation")?;
            self.string_id(at + 12, "support suffix-conjugation position")?;
            let flags = self.u8(at + 18, "support suffix-conjugation flags")?;
            if flags & 0xf0 != 0 {
                return corrupt("support suffix-conjugation flags are invalid");
            }
            self.tri(flags & 3)?;
            self.tri((flags >> 2) & 3)?;
            self.zero(
                at + 19,
                at + 24,
                "support suffix-conjugation reserved bytes",
            )?;
        }

        let mut prior_seq = None;
        for index in 0..self.count(SUFFIX_CLASSES) {
            let at = self.record(SUFFIX_CLASSES, index, "support suffix class")?;
            let seq = self.u32(at, "support suffix-class sequence")?;
            if prior_seq.is_some_and(|prior| prior >= seq) {
                return corrupt("support suffix classes are not canonically ordered");
            }
            prior_seq = Some(seq);
            self.string_id(at + 4, "support suffix-class keyword")?;
        }
        Ok(())
    }

    fn validate_counters(&self) -> Result<()> {
        let mut next_variant = 0;
        let mut prior_key = None;
        for index in 0..self.count(COUNTER_KEYS) {
            let at = self.record(COUNTER_KEYS, index, "support counter key")?;
            let key = self.string_id(at, "support counter-key string")?;
            if let Some(prior) = prior_key
                && self.compare_strings(prior, key)? != Ordering::Less
            {
                return corrupt("support counter keys are not canonically ordered");
            }
            prior_key = Some(key);
            let first = self.u32(at + 4, "support counter-variant start")? as usize;
            let count = self.u16(at + 8, "support counter-variant count")? as usize;
            if count == 0 || first != next_variant {
                return corrupt("support counter-variant spans are not canonical");
            }
            next_variant = self
                .span(
                    first,
                    count,
                    self.count(COUNTER_VARIANTS),
                    "support counter variants",
                )?
                .end;
            self.zero(at + 10, at + 12, "support counter-key reserved bytes")?;
        }
        if next_variant != self.count(COUNTER_VARIANTS) {
            return corrupt("support counter keys do not cover the variant table");
        }

        for index in 0..self.count(COUNTER_VARIANTS) {
            let at = self.record(COUNTER_VARIANTS, index, "support counter variant")?;
            self.string_id(at, "support counter text")?;
            self.string_id(at + 4, "support counter kana")?;
            self.validate_optional_string(self.u32(at + 8, "support counter suffix")?)?;
            let source_seq = self.u32(at + 12, "support counter source sequence")?;
            let source_text = self.u32(at + 16, "support counter source text")?;
            let source_route = self.u8(at + 51, "support counter source route")?;
            let source_ord = self.u16(at + 54, "support counter source ordinal")?;
            if source_seq == 0 {
                if source_text != NONE || source_route != 0 || source_ord != 0 {
                    return corrupt("empty support counter source is not canonical");
                }
            } else {
                if source_text == NONE {
                    return corrupt("support counter source text is missing");
                }
                if source_text as usize >= self.count(STRINGS) {
                    return corrupt("support counter source text is out of range");
                }
                self.string_slice(source_text as usize)?;
                self.route(source_route)?;
            }
            self.validate_span_fields(at + 20, at + 24, LIST_MEMBERS, "counter descriptions")?;
            self.validate_span_fields(at + 28, at + 32, DIGIT_OPTIONS, "counter digit options")?;
            self.validate_span_fields(
                at + 36,
                at + 40,
                super::NUMBER_MEMBERS,
                "counter digit set",
            )?;
            self.validate_span_fields(at + 44, at + 48, super::NUMBER_MEMBERS, "counter allowed")?;
            validate_counter_class(self.u8(at + 50, "support counter class")?)?;
            if self.u8(at + 52, "support counter flags")? & !3 != 0 {
                return corrupt("support counter flags are invalid");
            }
            self.zero(at + 26, at + 28, "support counter reserved bytes")?;
            self.zero(at + 34, at + 36, "support counter reserved bytes")?;
            self.zero(at + 42, at + 44, "support counter reserved bytes")?;
            self.zero(at + 56, at + 64, "support counter reserved bytes")?;
        }

        for index in 0..self.count(DIGIT_OPTIONS) {
            let at = self.record(DIGIT_OPTIONS, index, "support digit option")?;
            if self.i16(at, "support digit-option selector")? < -1 {
                return corrupt("support digit-option selector is invalid");
            }
            self.validate_span_fields(at + 4, at + 2, LIST_MEMBERS, "digit-option values")?;
            self.zero(at + 8, at + 12, "support digit-option reserved bytes")?;
        }
        Ok(())
    }

    fn validate_annotations(&self) -> Result<()> {
        let mut next_part = 0;
        for index in 0..self.count(SPLITS) {
            let at = self.record(SPLITS, index, "support split")?;
            self.string_id(at + 4, "support split surface")?;
            let first = self.u32(at + 8, "support split-part start")? as usize;
            let count = self.u16(at + 24, "support split-part count")? as usize;
            if first != next_part {
                return corrupt("support split-part spans are not canonical");
            }
            next_part = self
                .span(first, count, self.count(SPLIT_PARTS), "support split parts")?
                .end;
            self.validate_optional_string(self.u32(at + 16, "support split connector")?)?;
            self.validate_span_fields(at + 20, at + 26, super::NUMBER_MEMBERS, "split roots")?;
            self.route(self.u8(at + 29, "support split route")?)?;
            if self.u8(at + 30, "support split kind")? > 1 {
                return corrupt("support split kind is invalid");
            }
            self.zero(at + 31, at + 36, "support split reserved bytes")?;
            if index > 0 && self.compare_split_records(index - 1, index)? != Ordering::Less {
                return corrupt("support splits are not canonically ordered");
            }
        }
        if next_part != self.count(SPLIT_PARTS) {
            return corrupt("support splits do not cover the split-part table");
        }
        for index in 0..self.count(SPLIT_PARTS) {
            let at = self.record(SPLIT_PARTS, index, "support split part")?;
            match self.u8(at, "support split-part kind")? {
                0 => {
                    self.route(self.u8(at + 1, "support split-part route")?)?;
                    if self.u8(at + 2, "support split-part flags")? & !3 != 0 {
                        return corrupt("support split-part flags are invalid");
                    }
                    self.string_id(at + 8, "support split-part text")?;
                    self.validate_optional_string(self.u32(at + 12, "support split-part best")?)?;
                    self.string_id(at + 16, "support split-part common tags")?;
                    self.zero(at + 22, at + 28, "support split-part reserved bytes")?;
                }
                1 | 2 => self.zero(at + 1, at + 28, "support split marker reserved bytes")?,
                _ => return corrupt("support split-part kind is invalid"),
            }
        }

        for index in 0..self.count(HINTS) {
            let at = self.record(HINTS, index, "support hint")?;
            self.string_id(at + 4, "support hint surface")?;
            self.string_id(at + 8, "support hint reading")?;
            self.string_id(at + 12, "support hint text")?;
            self.route(self.u8(at + 16, "support hint route")?)?;
            self.zero(at + 17, at + 20, "support hint reserved bytes")?;
            if index > 0 && self.compare_hint_records(index - 1, index)? != Ordering::Less {
                return corrupt("support hints are not canonically ordered");
            }
        }

        for index in 0..self.count(COLLISIONS) {
            let at = self.record(COLLISIONS, index, "support collision")?;
            self.string_id(at + 8, "support collision surface")?;
            let second = self.u32(at + 16, "support collision second rule")?;
            if second != NONE && second == self.u32(at + 12, "support collision first rule")? {
                return corrupt("support collision rule path repeats one rule");
            }
            self.validate_span_fields(at + 24, at + 28, LIST_MEMBERS, "collision positions")?;
            let flags = self.u16(at + 30, "support collision flags")?;
            if flags & !0x07ff != 0 {
                return corrupt("support collision flags are invalid");
            }
            if index > 0 && self.compare_collision_records(index - 1, index)? != Ordering::Less {
                return corrupt("support collisions are not canonically ordered");
            }
        }
        Ok(())
    }

    fn validate_optional_string(&self, id: u32) -> Result<()> {
        if id != NONE {
            if id as usize >= self.count(STRINGS) {
                return corrupt("support optional string is out of range");
            }
            self.string_slice(id as usize)?;
        }
        Ok(())
    }

    fn validate_span_fields(
        &self,
        first_at: usize,
        count_at: usize,
        table: usize,
        label: &str,
    ) -> Result<()> {
        let first = self.u32(first_at, label)? as usize;
        let count = self.u16(count_at, label)? as usize;
        self.span(first, count, self.count(table), label).map(drop)
    }

    fn zero(&self, start: usize, end: usize, label: &str) -> Result<()> {
        assert_zero(&self.bytes, start, end, ErrorCode::CorruptPayload, label)
    }
}

fn corrupt<T>(message: &str) -> Result<T> {
    Err(KernelError::new(ErrorCode::CorruptPayload, message))
}
