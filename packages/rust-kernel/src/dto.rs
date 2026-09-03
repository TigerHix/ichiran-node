use serde::{Serialize, Serializer};
use serde_json::value::RawValue;

use crate::morphology::{MorphologyProperty, Route};

/// Lossless JavaScript text. Rust `String` cannot represent an unpaired UTF-16
/// surrogate, while JavaScript strings and the analyzer boundary can.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Utf16Text(Vec<u16>);

impl Utf16Text {
    pub fn from_units(value: &[u16]) -> Self {
        Self(value.to_vec())
    }

    pub fn from_string(value: String) -> Self {
        Self(value.encode_utf16().collect())
    }

    pub fn units(&self) -> &[u16] {
        &self.0
    }
}

impl From<String> for Utf16Text {
    fn from(value: String) -> Self {
        Self::from_string(value)
    }
}

impl From<&str> for Utf16Text {
    fn from(value: &str) -> Self {
        Self(value.encode_utf16().collect())
    }
}

impl Serialize for Utf16Text {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let raw = RawValue::from_string(quote_utf16(&self.0)).map_err(serde::ser::Error::custom)?;
        raw.serialize(serializer)
    }
}

fn quote_utf16(value: &[u16]) -> String {
    let mut output = String::with_capacity(value.len() + 2);
    output.push('"');
    let mut offset = 0;
    while offset < value.len() {
        let first = value[offset];
        match first {
            0x22 => output.push_str("\\\""),
            0x5c => output.push_str("\\\\"),
            0x08 => output.push_str("\\b"),
            0x09 => output.push_str("\\t"),
            0x0a => output.push_str("\\n"),
            0x0c => output.push_str("\\f"),
            0x0d => output.push_str("\\r"),
            0x00..=0x1f => push_json_escape(&mut output, first),
            0xd800..=0xdbff if offset + 1 < value.len() => {
                let second = value[offset + 1];
                if (0xdc00..=0xdfff).contains(&second) {
                    let scalar =
                        0x1_0000 + ((u32::from(first) - 0xd800) << 10) + u32::from(second) - 0xdc00;
                    if let Some(character) = char::from_u32(scalar) {
                        output.push(character);
                    } else {
                        push_json_escape(&mut output, first);
                        push_json_escape(&mut output, second);
                    }
                    offset += 1;
                } else {
                    push_json_escape(&mut output, first);
                }
            }
            0xd800..=0xdfff => push_json_escape(&mut output, first),
            _ => {
                if let Some(character) = char::from_u32(u32::from(first)) {
                    output.push(character);
                } else {
                    push_json_escape(&mut output, first);
                }
            }
        }
        offset += 1;
    }
    output.push('"');
    output
}

fn push_json_escape(output: &mut String, unit: u16) {
    const HEX: &[u8; 16] = b"0123456789abcdef";
    output.push_str("\\u");
    output.push(HEX[usize::from((unit >> 12) & 0xf)] as char);
    output.push(HEX[usize::from((unit >> 8) & 0xf)] as char);
    output.push(HEX[usize::from((unit >> 4) & 0xf)] as char);
    output.push(HEX[usize::from(unit & 0xf)] as char);
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisResult {
    pub input: Utf16Text,
    pub normalized: Utf16Text,
    #[serde(serialize_with = "serialize_js_number")]
    pub compute_ms: f64,
    pub chunks: Vec<AnalysisChunk>,
    pub paths: Vec<AnalysisPath>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum AnalysisChunk {
    Misc {
        start: usize,
        end: usize,
        text: Utf16Text,
    },
    Word {
        start: usize,
        end: usize,
        text: Utf16Text,
        paths: Vec<AnalysisPath>,
    },
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct AnalysisPath {
    #[serde(serialize_with = "serialize_js_number")]
    pub score: f64,
    pub tokens: Vec<AnalysisToken>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisToken {
    pub candidate_id: Option<i64>,
    pub start: usize,
    pub end: usize,
    pub text: Utf16Text,
    pub true_text: Option<Utf16Text>,
    pub route: PublicRoute,
    pub reading: Utf16Text,
    pub romanized: Utf16Text,
    pub pos: Vec<String>,
    #[serde(serialize_with = "serialize_js_number")]
    pub score: f64,
    pub entry_index: Option<usize>,
    pub root: Option<AnalysisRoot>,
    pub inflection: Vec<MorphologyProperty>,
    pub components: Vec<AnalysisComponent>,
    pub alternatives: Vec<AnalysisAlternative>,
    pub skipped: usize,
    pub entity: bool,
    pub counter: Option<(String, bool)>,
    #[serde(skip)]
    pub(crate) legacy: Option<LegacyPresentationFacts>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisAlternative {
    pub candidate_id: i64,
    pub text: Utf16Text,
    pub true_text: Option<Utf16Text>,
    pub route: Route,
    pub reading: Utf16Text,
    pub romanized: Utf16Text,
    pub pos: Vec<String>,
    #[serde(serialize_with = "serialize_js_number")]
    pub score: f64,
    pub entry_index: Option<usize>,
    pub root: Option<AnalysisRoot>,
    pub inflection: Vec<MorphologyProperty>,
    pub components: Vec<AnalysisComponent>,
    pub counter: Option<(String, bool)>,
    #[serde(skip)]
    pub(crate) legacy: Option<LegacyPresentationFacts>,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisComponent {
    pub text: Utf16Text,
    pub true_text: Option<Utf16Text>,
    pub route: Route,
    pub reading: Utf16Text,
    pub entry_index: Option<usize>,
    pub root: Option<AnalysisRoot>,
    pub inflection: Vec<MorphologyProperty>,
    pub primary: bool,
    #[serde(skip)]
    pub(crate) legacy: Option<LegacyPresentationFacts>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct AnalysisRoot {
    pub seq: u32,
    pub form: String,
    pub reading: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub(crate) enum LegacyConjugationSelection {
    Default,
    Explicit,
    Root,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LegacySemanticMember {
    pub entry_index: Option<usize>,
    pub root: Option<AnalysisRoot>,
    pub inflection: Vec<MorphologyProperty>,
    pub stage_groups: Vec<Option<u32>>,
    pub stage_keys: Vec<Option<String>>,
    pub stage_member_ords: Vec<Option<u8>>,
    pub stage_prop_ords: Vec<Option<u16>>,
    pub member_ord: Option<u8>,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct LegacyPresentationFacts {
    pub physical_group: Option<u32>,
    pub suffix_class: Option<String>,
    pub definition_seq: Option<u32>,
    pub semantic_members: Vec<LegacySemanticMember>,
    pub identity_roots: Vec<u32>,
    pub conjugation_selection: LegacyConjugationSelection,
    pub contextual_reading: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum PublicRoute {
    Kana,
    Kanji,
    Gap,
}

impl From<Route> for PublicRoute {
    fn from(value: Route) -> Self {
        match value {
            Route::Kana => Self::Kana,
            Route::Kanji => Self::Kanji,
        }
    }
}

pub(crate) fn serialize_js_number<S>(value: &f64, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    if !value.is_finite() {
        return Err(serde::ser::Error::custom("analyzer numbers must be finite"));
    }
    if value.fract() == 0.0 && *value >= i64::MIN as f64 && *value <= i64::MAX as f64 {
        serializer.serialize_i64(*value as i64)
    } else {
        serializer.serialize_f64(*value)
    }
}

#[cfg(test)]
mod tests {
    use serde::Serialize;

    use super::{AnalysisPath, Utf16Text};

    #[test]
    fn serializes_javascript_utf16_losslessly() {
        let value = Utf16Text::from_units(&[0x732b, 0xd83d, 0x72ac, 0xde00, 0xd83d, 0xde00]);
        assert_eq!(
            serde_json::to_string(&value).unwrap(),
            "\"猫\\ud83d犬\\ude00😀\""
        );
    }

    #[test]
    fn serializes_scores_like_javascript_json_numbers() {
        #[derive(Serialize)]
        struct Values {
            integral: AnalysisPath,
            fractional: AnalysisPath,
            negative_zero: AnalysisPath,
        }
        let value = Values {
            integral: AnalysisPath {
                score: 19.0,
                tokens: Vec::new(),
            },
            fractional: AnalysisPath {
                score: 2.5,
                tokens: Vec::new(),
            },
            negative_zero: AnalysisPath {
                score: -0.0,
                tokens: Vec::new(),
            },
        };
        assert_eq!(
            serde_json::to_string(&value).unwrap(),
            r#"{"integral":{"score":19,"tokens":[]},"fractional":{"score":2.5,"tokens":[]},"negative_zero":{"score":0,"tokens":[]}}"#
        );
    }
}
