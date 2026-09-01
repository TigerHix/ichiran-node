//! Japanese number conversion with the same grouping and reading rules as the
//! portable TypeScript analyzer.

use std::fmt;

use crate::characters::{geminate, rendaku};

pub const DIGIT_KANJI_DEFAULT: &str = "〇一二三四五六七八九";
pub const DIGIT_KANJI_LEGAL: &str = "〇壱弐参四五六七八九拾";
pub const POWER_KANJI: &str = "一十百千万   億   兆   京";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum NumberType {
    JapaneseDigit,
    Power,
    ArabicDigit,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct NumberClass {
    kind: NumberType,
    value: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NumberToKanjiError;

impl fmt::Display for NumberToKanjiError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("Number must be a non-negative integer")
    }
}

impl std::error::Error for NumberToKanjiError {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NotANumberError {
    pub text: Vec<u16>,
    pub reason: Vec<u16>,
}

impl NotANumberError {
    pub fn message_utf16(&self) -> Vec<u16> {
        let mut message: Vec<u16> = "\"".encode_utf16().collect();
        message.extend_from_slice(&self.text);
        message.extend("\" is not a number: ".encode_utf16());
        message.extend_from_slice(&self.reason);
        message
    }
}

impl fmt::Display for NotANumberError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&String::from_utf16_lossy(&self.message_utf16()))
    }
}

impl std::error::Error for NotANumberError {}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum NumberKana {
    Joined(Vec<u16>),
    Groups(Vec<Vec<u16>>),
}

fn number_class(unit: u16) -> Option<NumberClass> {
    let digit = |kind, value| Some(NumberClass { kind, value });
    match unit {
        0x3007 | 0x96f6 => digit(NumberType::JapaneseDigit, 0),
        0x4e00 | 0x58f1 => digit(NumberType::JapaneseDigit, 1),
        0x4e8c | 0x5f10 => digit(NumberType::JapaneseDigit, 2),
        0x4e09 | 0x53c2 => digit(NumberType::JapaneseDigit, 3),
        0x56db => digit(NumberType::JapaneseDigit, 4),
        0x4e94 => digit(NumberType::JapaneseDigit, 5),
        0x516d => digit(NumberType::JapaneseDigit, 6),
        0x4e03 => digit(NumberType::JapaneseDigit, 7),
        0x516b => digit(NumberType::JapaneseDigit, 8),
        0x4e5d => digit(NumberType::JapaneseDigit, 9),
        0x5341 | 0x62fe => digit(NumberType::Power, 1),
        0x767e => digit(NumberType::Power, 2),
        0x5343 => digit(NumberType::Power, 3),
        0x4e07 => digit(NumberType::Power, 4),
        0x5104 => digit(NumberType::Power, 8),
        0x5146 => digit(NumberType::Power, 12),
        0x4eac => digit(NumberType::Power, 16),
        0x30..=0x39 => digit(NumberType::ArabicDigit, u32::from(unit - 0x30)),
        0xff10..=0xff19 => digit(NumberType::ArabicDigit, u32::from(unit - 0xff10)),
        _ => None,
    }
}

pub fn number_to_kanji(number: f64) -> Result<Vec<u16>, NumberToKanjiError> {
    number_to_kanji_with(
        number,
        &DIGIT_KANJI_DEFAULT.encode_utf16().collect::<Vec<_>>(),
        &POWER_KANJI.encode_utf16().collect::<Vec<_>>(),
        false,
    )
}

pub fn number_to_kanji_with(
    number: f64,
    digits: &[u16],
    powers: &[u16],
    one_sen: bool,
) -> Result<Vec<u16>, NumberToKanjiError> {
    if !number.is_finite() || number < 0.0 || number.fract() != 0.0 {
        return Err(NumberToKanjiError);
    }
    number_to_kanji_inner(number, digits, powers, one_sen)
}

fn number_to_kanji_inner(
    number: f64,
    digits: &[u16],
    powers: &[u16],
    one_sen: bool,
) -> Result<Vec<u16>, NumberToKanjiError> {
    if number == 0.0 {
        return digits
            .first()
            .copied()
            .map(|unit| vec![unit])
            .ok_or(NumberToKanjiError);
    }

    let mut magnitude = 1.0;
    let mut magnitude_character = None;
    let mut power = 1.0;
    for character in powers {
        if power > number {
            break;
        }
        if *character != b' ' as u16 {
            magnitude = power;
            magnitude_character = Some(*character);
        }
        power *= 10.0;
    }
    if magnitude == 1.0 {
        return digits
            .get(number as usize)
            .copied()
            .map(|unit| vec![unit])
            .ok_or(NumberToKanjiError);
    }

    let quotient = (number / magnitude).floor();
    let remainder = number % magnitude;
    let mut output = if quotient == 1.0 && magnitude <= if one_sen { 100.0 } else { 1000.0 } {
        Vec::new()
    } else {
        number_to_kanji_inner(quotient, digits, powers, true)?
    };
    output.push(magnitude_character.ok_or(NumberToKanjiError)?);
    if remainder != 0.0 {
        output.extend(number_to_kanji_inner(remainder, digits, powers, one_sen)?);
    }
    Ok(output)
}

fn parse_number_classes(classes: &[NumberClass]) -> f64 {
    let Some((greatest_index, greatest_power)) = classes
        .iter()
        .enumerate()
        .filter(|(_, item)| item.kind == NumberType::Power)
        .fold(None, |greatest, (index, item)| match greatest {
            Some((_, power)) if power >= item.value => greatest,
            _ => Some((index, item.value)),
        })
    else {
        return classes
            .iter()
            .fold(0.0, |number, item| number * 10.0 + f64::from(item.value));
    };

    let power = 10_f64.powi(greatest_power as i32);
    if greatest_index == 0 {
        return power + parse_number_classes(&classes[1..]);
    }
    parse_number_classes(&classes[..greatest_index]) * power
        + parse_number_classes(&classes[greatest_index + 1..])
}

fn next_js_character(input: &[u16], offset: usize) -> usize {
    if (0xd800..=0xdbff).contains(&input[offset])
        && input
            .get(offset + 1)
            .is_some_and(|unit| (0xdc00..=0xdfff).contains(unit))
    {
        2
    } else {
        1
    }
}

pub fn parse_number(input: &[u16]) -> Result<f64, NotANumberError> {
    let mut classes = Vec::new();
    let mut offset = 0;
    while offset < input.len() {
        let width = next_js_character(input, offset);
        if width == 1
            && let Some(class) = number_class(input[offset])
        {
            classes.push(class);
            offset += 1;
            continue;
        }
        let character = &input[offset..offset + width];
        let mut reason: Vec<u16> = "Invalid character: ".encode_utf16().collect();
        reason.extend_from_slice(character);
        return Err(NotANumberError {
            text: input.to_vec(),
            reason,
        });
    }
    Ok(parse_number_classes(&classes))
}

fn digit_kana(value: u32) -> &'static str {
    match value {
        0 => "れい",
        1 => "いち",
        2 => "に",
        3 => "さん",
        4 => "よん",
        5 => "ご",
        6 => "ろく",
        7 => "なな",
        8 => "はち",
        9 => "きゅう",
        _ => "",
    }
}

fn power_kana(value: u32) -> &'static str {
    match value {
        1 => "じゅう",
        2 => "ひゃく",
        3 => "せん",
        4 => "まん",
        8 => "おく",
        12 => "ちょう",
        16 => "けい",
        _ => "",
    }
}

fn join_number_kana(
    previous: Option<NumberClass>,
    item: NumberClass,
    prefix: &mut Vec<u16>,
    suffix: &mut Vec<u16>,
) {
    if let Some(previous) = previous {
        match (previous.kind, previous.value, item.kind, item.value) {
            (NumberType::JapaneseDigit, 1, NumberType::Power, 3 | 12 | 16)
            | (NumberType::JapaneseDigit, 6, NumberType::Power, 16)
            | (NumberType::JapaneseDigit, 8, NumberType::Power, 3 | 12 | 16)
            | (NumberType::Power, 1, NumberType::Power, 12 | 16)
            | (NumberType::Power, 2, NumberType::Power, 16) => *prefix = geminate(prefix),
            (NumberType::JapaneseDigit, 3, NumberType::Power, 2 | 3) => {
                *suffix = rendaku(suffix, false)
            }
            (NumberType::JapaneseDigit, 6 | 8, NumberType::Power, 2) => {
                *prefix = geminate(prefix);
                *suffix = rendaku(suffix, true);
            }
            _ => {}
        }
    }
    prefix.append(suffix);
}

fn group_to_kana(group: &[NumberClass]) -> Vec<u16> {
    let mut output = Vec::new();
    let mut previous = None;
    for item in group {
        let kana = match item.kind {
            NumberType::JapaneseDigit => digit_kana(item.value),
            NumberType::Power => power_kana(item.value),
            NumberType::ArabicDigit => "",
        };
        let mut suffix: Vec<u16> = kana.encode_utf16().collect();
        join_number_kana(previous, *item, &mut output, &mut suffix);
        previous = Some(*item);
    }
    output
}

fn kana_groups(kanji: &[u16]) -> Vec<Vec<u16>> {
    let mut groups: Vec<Vec<NumberClass>> = Vec::new();
    let mut group = Vec::new();
    let mut previous = None;
    for character in kanji {
        let Some(item) = number_class(*character) else {
            continue;
        };
        let continues = previous.is_none_or(|previous: NumberClass| {
            item.kind == NumberType::Power
                && (previous.kind == NumberType::JapaneseDigit
                    || (previous.kind == NumberType::Power && item.value > previous.value))
        });
        if !continues {
            groups.push(std::mem::take(&mut group));
        }
        group.push(item);
        previous = Some(item);
    }
    if !group.is_empty() {
        groups.push(group);
    }
    groups.iter().map(|items| group_to_kana(items)).collect()
}

pub fn number_to_kana(number: f64) -> Result<Vec<u16>, NumberToKanjiError> {
    match number_to_kana_with_separator(number, Some(&[b' ' as u16]))? {
        NumberKana::Joined(value) => Ok(value),
        NumberKana::Groups(groups) => {
            Ok(groups
                .into_iter()
                .enumerate()
                .fold(Vec::new(), |mut joined, (index, group)| {
                    if index > 0 {
                        joined.push(b' ' as u16);
                    }
                    joined.extend(group);
                    joined
                }))
        }
    }
}

pub fn number_to_kana_with_separator(
    number: f64,
    separator: Option<&[u16]>,
) -> Result<NumberKana, NumberToKanjiError> {
    let kanji = number_to_kanji(number)?;
    Ok(number_to_kana_from_kanji(&kanji, separator))
}

pub fn number_to_kana_from_kanji(kanji: &[u16], separator: Option<&[u16]>) -> NumberKana {
    let readings = kana_groups(kanji);
    let Some(separator) = separator else {
        return NumberKana::Groups(readings);
    };
    let mut output = Vec::new();
    for (index, reading) in readings.iter().enumerate() {
        if index > 0 {
            output.extend_from_slice(separator);
        }
        output.extend_from_slice(reading);
    }
    NumberKana::Joined(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn u(value: &str) -> Vec<u16> {
        value.encode_utf16().collect()
    }

    #[test]
    fn preserves_exact_error_text_in_utf16() {
        let error = parse_number(&[0x4e00, 0xd83d, 0xde00]).unwrap_err();
        let mut expected = u("\"一");
        expected.extend([0xd83d, 0xde00]);
        expected.extend(u("\" is not a number: Invalid character: "));
        expected.extend([0xd83d, 0xde00]);
        assert_eq!(error.message_utf16(), expected);
        assert_eq!(
            NumberToKanjiError.to_string(),
            "Number must be a non-negative integer"
        );
        assert_eq!(
            number_to_kanji_with(
                0.0,
                &[0xd83d],
                &POWER_KANJI.encode_utf16().collect::<Vec<_>>(),
                false,
            )
            .unwrap(),
            vec![0xd83d],
        );
    }
}
