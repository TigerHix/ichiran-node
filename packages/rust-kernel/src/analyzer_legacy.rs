use serde::Serialize;
use serde_json::Value;

use crate::dto::{
    AnalysisResult, AnalysisToken, LegacyConjugationSelection, LegacyPresentationFacts, Utf16Text,
    serialize_js_number,
};
use crate::morphology::MorphologyProperty;
use crate::romanization::{RomanizationName, romanize_word};

mod compact;
mod conjugations;
mod descriptions;
mod detailed;
mod senses;
#[cfg(test)]
mod tests;

pub(crate) use detailed::{LegacyContext, LegacyDetailedResult, LegacyDetailedSession};

pub(crate) type LegacyWordProperty<'a> = dyn Fn(&[u16], &AnalysisToken) -> Value + 'a;

#[derive(Default)]
pub(crate) struct LegacyOptions<'a> {
    pub method: Option<RomanizationName>,
    pub word_property: Option<&'a LegacyWordProperty<'a>>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum LegacyChunk<W> {
    Misc(Utf16Text),
    Paths(Vec<LegacyPath<W>>),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct LegacyPath<W>(
    pub Vec<LegacyToken<W>>,
    #[serde(serialize_with = "serialize_js_number")] pub f64,
);

#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct LegacyToken<W>(pub Utf16Text, pub W, pub Value);

pub(crate) type LegacyCompactResult = Vec<LegacyChunk<LegacyWordInfo>>;
pub(crate) type LegacyDetailedOutput = Vec<LegacyChunk<LegacyGloss>>;

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub(crate) enum LegacyWordType {
    #[serde(rename = "KANJI")]
    Kanji,
    #[serde(rename = "KANA")]
    Kana,
    #[serde(rename = "GAP")]
    Gap,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum LegacyReading {
    One(Utf16Text),
    Many(Vec<Utf16Text>),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum LegacySequence {
    One(u32),
    Many(Vec<u32>),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct LegacyWordInfo {
    #[serde(rename = "type")]
    pub kind: LegacyWordType,
    pub text: Utf16Text,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub truetext: Option<Utf16Text>,
    pub kana: LegacyReading,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub seq: Option<LegacySequence>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conjugations: Option<Vec<MorphologyProperty>>,
    #[serde(serialize_with = "serialize_js_number")]
    pub score: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Vec<LegacyWordInfo>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alternative: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub primary: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub counter: Option<(String, bool)>,
    pub skipped: usize,
    #[serde(rename = "isEntity", skip_serializing_if = "Option::is_none")]
    pub is_entity: Option<bool>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct LegacySense {
    pub pos: String,
    pub gloss: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub field: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub info: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct LegacyConjugationProperty {
    pub pos: String,
    #[serde(rename = "type")]
    pub kind: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fml: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub neg: Option<bool>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct LegacyConjugationFlags {
    pub negative: Option<bool>,
    pub formal: Option<bool>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct LegacyConjugationInfo {
    pub flags: Vec<LegacyConjugationFlags>,
    pub short_gloss: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct LegacyConjugation {
    pub prop: Vec<LegacyConjugationProperty>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reading: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gloss: Option<Vec<LegacySense>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub via: Option<Vec<LegacyConjugation>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub readok: Option<bool>,
    #[serde(skip)]
    pub info: Option<LegacyConjugationInfo>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub(crate) struct LegacyCounter {
    pub value: String,
    pub ordinal: LegacyOrdinal,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(untagged)]
pub(crate) enum LegacyOrdinal {
    Yes(bool),
    No(Vec<()>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct LegacyWordFacts {
    pub definition_seq: Option<u32>,
    pub conjugation_selection: LegacyConjugationSelection,
    pub inflected: bool,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub(crate) struct LegacyGloss {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reading: Option<Utf16Text>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<Utf16Text>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kana: Option<Utf16Text>,
    #[serde(
        skip_serializing_if = "Option::is_none",
        serialize_with = "serialize_optional_js_number"
    )]
    pub score: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compound: Option<Vec<Utf16Text>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Vec<LegacyGloss>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub counter: Option<LegacyCounter>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub seq: Option<LegacySequence>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gloss: Option<Vec<LegacySense>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suffix: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conj: Option<Vec<LegacyConjugation>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alternative: Option<Vec<LegacyGloss>>,
    #[serde(skip)]
    pub info: Option<LegacyWordFacts>,
}

impl LegacyGloss {
    fn alternative(values: Vec<Self>) -> Self {
        Self {
            reading: None,
            text: None,
            kana: None,
            score: None,
            compound: None,
            components: None,
            counter: None,
            seq: None,
            gloss: None,
            suffix: None,
            conj: None,
            alternative: Some(values),
            info: None,
        }
    }
}

fn serialize_optional_js_number<S>(value: &Option<f64>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    match value {
        Some(value) => serialize_js_number(value, serializer),
        None => serializer.serialize_none(),
    }
}

fn facts(token: &AnalysisToken) -> Option<&LegacyPresentationFacts> {
    token.legacy.as_ref()
}

fn token_romanized(token: &AnalysisToken, method: Option<RomanizationName>) -> Utf16Text {
    let method = method.unwrap_or(RomanizationName::HepburnTraditional);
    if token.alternatives.len() > 1 && !facts(token).is_some_and(|value| value.contextual_reading) {
        return Utf16Text::from_units(&romanized_readings(
            &token
                .alternatives
                .iter()
                .map(|value| value.reading.units())
                .collect::<Vec<_>>(),
            token.text.units(),
            method,
        ));
    }
    Utf16Text::from_units(&romanize_word(
        token.reading.units(),
        method,
        Some(token.text.units()),
        true,
    ))
}

fn romanized_readings(readings: &[&[u16]], text: &[u16], method: RomanizationName) -> Vec<u16> {
    let values = readings
        .iter()
        .map(|reading| romanize_word(reading, method, Some(text), true))
        .collect::<Vec<_>>();
    simplify_reading_list(&values)
        .into_iter()
        .enumerate()
        .flat_map(|(index, value)| {
            let separator = (index > 0).then_some(b'/' as u16);
            separator.into_iter().chain(value)
        })
        .collect()
}

fn simplify_reading_list(readings: &[Vec<u16>]) -> Vec<Vec<u16>> {
    let mut values: Vec<(Vec<u16>, usize, Vec<usize>)> = Vec::new();
    for reading in readings {
        let mut text = Vec::new();
        let mut spaces = Vec::new();
        for unit in reading {
            if *unit == b' ' as u16 {
                spaces.push(text.len());
            } else {
                text.push(*unit);
            }
        }
        if let Some(current) = values.iter_mut().find(|value| value.0 == text) {
            current.1 += 1;
            current.2.extend(spaces);
        } else {
            values.push((text, 1, spaces));
        }
    }
    values
        .into_iter()
        .map(|(text, count, spaces)| {
            let mut output = Vec::new();
            for (index, unit) in text.into_iter().enumerate() {
                let occurrences = spaces.iter().filter(|position| **position == index).count();
                if occurrences > 0 {
                    output.push(if occurrences == count { 0x20 } else { 0xb7 });
                }
                output.push(unit);
            }
            output
        })
        .collect()
}

pub(crate) fn serialize_compact(
    result: &AnalysisResult,
    options: &LegacyOptions<'_>,
) -> LegacyCompactResult {
    compact::serialize(result, options)
}
