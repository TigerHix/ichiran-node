use crate::error::{ErrorCode, KernelError, Result};

use super::{
    AnalyzerSupport, NONE, SUFFIX_CLASSES, SUFFIX_CONJUGATIONS, SUFFIX_FORMS, SUFFIX_KEYS,
    SUFFIX_VALUES, SupportConjugation, SupportConjugationProperty, SupportConjugations,
    SupportSuffixForm, SupportSuffixMatch, SupportSuffixValue,
};

impl AnalyzerSupport {
    pub fn suffix(&self, text: &[u16]) -> Result<Vec<SupportSuffixValue>> {
        let Some(index) = self.find_string_key(text, SUFFIX_KEYS)? else {
            return Ok(Vec::new());
        };
        let at = self.record(SUFFIX_KEYS, index, "support suffix key")?;
        let first = self.u32(at + 4, "support suffix-value start")? as usize;
        let count = self.u16(at + 8, "support suffix-value count")? as usize;
        self.span(
            first,
            count,
            self.count(SUFFIX_VALUES),
            "support suffix values",
        )?
        .map(|index| self.suffix_value(index))
        .collect()
    }

    pub fn suffix_matches_ending_at(
        &self,
        text: &[u16],
        end: usize,
        max_code_units: usize,
    ) -> Result<Vec<SupportSuffixMatch>> {
        if end > text.len() {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                format!("suffix end {end} lies outside the input"),
            ));
        }
        let mut output = Vec::new();
        for &length in &self.suffix_lengths {
            if length > max_code_units || length > end {
                continue;
            }
            let start = end - length;
            let value = &text[start..end];
            let values = self.suffix(value)?;
            if values.is_empty() {
                continue;
            }
            let matched = String::from_utf16(value).map_err(|_| {
                KernelError::new(
                    ErrorCode::Internal,
                    "a malformed UTF-16 suffix matched a valid support key",
                )
            })?;
            output.push(SupportSuffixMatch {
                start,
                end,
                text: matched,
                values,
            });
        }
        Ok(output)
    }

    pub fn suffix_class(&self, seq: u32) -> Result<Option<String>> {
        let mut low = 0;
        let mut high = self.count(SUFFIX_CLASSES);
        while low < high {
            let middle = low + (high - low) / 2;
            let at = self.record(SUFFIX_CLASSES, middle, "support suffix class")?;
            if self.u32(at, "support suffix-class sequence")? < seq {
                low = middle + 1;
            } else {
                high = middle;
            }
        }
        if low >= self.count(SUFFIX_CLASSES) {
            return Ok(None);
        }
        let at = self.record(SUFFIX_CLASSES, low, "support suffix class")?;
        if self.u32(at, "support suffix-class sequence")? != seq {
            return Ok(None);
        }
        self.string(self.string_id(at + 4, "support suffix-class keyword")?)
            .map(Some)
    }

    fn suffix_value(&self, index: usize) -> Result<SupportSuffixValue> {
        let at = self.record(SUFFIX_VALUES, index, "support suffix value")?;
        let keyword = self.string(self.string_id(at, "support suffix keyword")?)?;
        let form = self.u32(at + 4, "support suffix form")?;
        Ok(SupportSuffixValue {
            keyword,
            form: if form == NONE {
                None
            } else {
                Some(self.suffix_form(form as usize)?)
            },
        })
    }

    fn suffix_form(&self, index: usize) -> Result<SupportSuffixForm> {
        let at = self.record(SUFFIX_FORMS, index, "support suffix form")?;
        let first = self.u32(at + 16, "support suffix-conjugation start")? as usize;
        let count = self.u16(at + 22, "support suffix-conjugation count")? as usize;
        let flags = self.u8(at + 25, "support suffix-form flags")?;
        let conjugations = if flags & 4 != 0 {
            Some(SupportConjugations::Root)
        } else if count == 0 {
            None
        } else {
            let values = self
                .span(
                    first,
                    count,
                    self.count(SUFFIX_CONJUGATIONS),
                    "support suffix conjugations",
                )?
                .map(|index| self.suffix_conjugation(index))
                .collect::<Result<Vec<_>>>()?;
            Some(SupportConjugations::Values(values))
        };
        let best = self.u32(at + 8, "support suffix best kanji")?;
        let common = self.u8(at + 24, "support suffix common rank")?;
        Ok(SupportSuffixForm {
            seq: self.u32(at, "support suffix-form sequence")?,
            text: self.string(self.string_id(at + 4, "support suffix-form text")?)?,
            best_kanji: self.optional_string(best)?,
            common_tags: self.string(self.string_id(at + 12, "support suffix common tags")?)?,
            ord: self.u16(at + 20, "support suffix-form ordinal")?,
            common: (common != 0xff).then_some(common),
            conjugatable: flags & 1 != 0,
            nokanji: flags & 2 != 0,
            conjugations,
        })
    }

    fn suffix_conjugation(&self, index: usize) -> Result<SupportConjugation> {
        let at = self.record(SUFFIX_CONJUGATIONS, index, "support suffix conjugation")?;
        let via = self.u32(at + 8, "support suffix-conjugation via")?;
        let flags = self.u8(at + 18, "support suffix-conjugation flags")?;
        Ok(SupportConjugation {
            seq: self.u32(at, "support suffix-conjugation sequence")?,
            from: self.u32(at + 4, "support suffix-conjugation source")?,
            via: (via != NONE).then_some(via),
            property: SupportConjugationProperty {
                pos: self.string(self.string_id(at + 12, "support suffix position")?)?,
                kind: self.u16(at + 16, "support suffix-conjugation type")?,
                negative: self.tri(flags & 3)?,
                formal: self.tri((flags >> 2) & 3)?,
            },
        })
    }
}
