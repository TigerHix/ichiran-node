use crate::error::{ErrorCode, KernelError, Result};

use super::{
    AnalyzerSupport, COUNTER_KEYS, COUNTER_VARIANTS, DIGIT_OPTIONS, SupportCounterClass,
    SupportCounterMatch, SupportCounterSource, SupportCounterVariant, SupportDigit,
    SupportDigitOption,
};

impl AnalyzerSupport {
    pub fn counters(&self, text: &[u16]) -> Result<Vec<SupportCounterVariant>> {
        let Some(index) = self.find_string_key(text, COUNTER_KEYS)? else {
            return Ok(Vec::new());
        };
        let at = self.record(COUNTER_KEYS, index, "support counter key")?;
        let first = self.u32(at + 4, "support counter-variant start")? as usize;
        let count = self.u16(at + 8, "support counter-variant count")? as usize;
        self.span(
            first,
            count,
            self.count(COUNTER_VARIANTS),
            "support counter variants",
        )?
        .map(|index| self.counter(index))
        .collect()
    }

    pub fn counter_matches_starting_at(
        &self,
        text: &[u16],
        start: usize,
        max_code_units: usize,
    ) -> Result<Vec<SupportCounterMatch>> {
        if start > text.len() {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                format!("counter start {start} lies outside the input"),
            ));
        }
        let mut output = Vec::new();
        for &length in &self.counter_lengths {
            if length > max_code_units {
                continue;
            }
            let Some(end) = start.checked_add(length) else {
                continue;
            };
            if end > text.len() {
                continue;
            }
            let value = &text[start..end];
            let values = self.counters(value)?;
            if values.is_empty() {
                continue;
            }
            let matched = String::from_utf16(value).map_err(|_| {
                KernelError::new(
                    ErrorCode::Internal,
                    "a malformed UTF-16 counter matched a valid support key",
                )
            })?;
            output.push(SupportCounterMatch {
                start,
                end,
                text: matched,
                values,
            });
        }
        Ok(output)
    }

    fn counter(&self, index: usize) -> Result<SupportCounterVariant> {
        let at = self.record(COUNTER_VARIANTS, index, "support counter variant")?;
        let suffix = self.u32(at + 8, "support counter suffix")?;
        let source_seq = self.u32(at + 12, "support counter source sequence")?;
        let flags = self.u8(at + 52, "support counter flags")?;
        let common = self.u8(at + 53, "support counter common rank")?;
        let source = if source_seq == 0 {
            None
        } else {
            Some(SupportCounterSource {
                seq: source_seq,
                route: self.route(self.u8(at + 51, "support counter source route")?)?,
                text: self.string(self.string_id(at + 16, "support counter source text")?)?,
                ord: self.u16(at + 54, "support counter source ordinal")?,
            })
        };
        Ok(SupportCounterVariant {
            class_name: counter_class(self.u8(at + 50, "support counter class")?)?,
            text: self.string(self.string_id(at, "support counter text")?)?,
            kana: self.string(self.string_id(at + 4, "support counter kana")?)?,
            suffix: self.optional_string(suffix)?,
            source,
            ordinal: flags & 1 != 0,
            foreign: flags & 2 != 0,
            common: (common != 0xff).then_some(common),
            suffix_descriptions: self.string_list(
                self.u32(at + 20, "support counter description start")? as usize,
                self.u16(at + 24, "support counter description count")? as usize,
            )?,
            digit_options: self.digit_options(
                self.u32(at + 28, "support counter digit-option start")? as usize,
                self.u16(at + 32, "support counter digit-option count")? as usize,
            )?,
            digit_set: self.number_list(
                self.u32(at + 36, "support counter digit-set start")? as usize,
                self.u16(at + 40, "support counter digit-set count")? as usize,
            )?,
            allowed: self.number_list(
                self.u32(at + 44, "support counter allowed start")? as usize,
                self.u16(at + 48, "support counter allowed count")? as usize,
            )?,
        })
    }

    fn digit_options(&self, first: usize, count: usize) -> Result<Vec<SupportDigitOption>> {
        self.span(
            first,
            count,
            self.count(DIGIT_OPTIONS),
            "support counter digit options",
        )?
        .map(|index| {
            let at = self.record(DIGIT_OPTIONS, index, "support digit option")?;
            let digit = self.i16(at, "support digit-option selector")?;
            Ok(SupportDigitOption {
                digit: if digit == -1 {
                    SupportDigit::Off
                } else {
                    SupportDigit::Digit(digit)
                },
                values: self.string_list(
                    self.u32(at + 4, "support digit-option value start")? as usize,
                    self.u16(at + 2, "support digit-option value count")? as usize,
                )?,
            })
        })
        .collect()
    }
}

fn counter_class(code: u8) -> Result<SupportCounterClass> {
    const VALUES: [SupportCounterClass; 11] = [
        SupportCounterClass::CounterText,
        SupportCounterClass::NumberText,
        SupportCounterClass::CounterHalfhour,
        SupportCounterClass::CounterTsu,
        SupportCounterClass::CounterHifumi,
        SupportCounterClass::CounterDaysKun,
        SupportCounterClass::CounterDaysOn,
        SupportCounterClass::CounterMonths,
        SupportCounterClass::CounterPeople,
        SupportCounterClass::CounterWari,
        SupportCounterClass::CounterAge,
    ];
    VALUES.get(code as usize).copied().ok_or_else(|| {
        KernelError::new(
            ErrorCode::CorruptPayload,
            format!("invalid support counter class {code}"),
        )
    })
}

pub(super) fn validate_counter_class(code: u8) -> Result<()> {
    counter_class(code).map(drop)
}
