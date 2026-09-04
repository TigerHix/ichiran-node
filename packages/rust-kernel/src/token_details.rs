use serde::Serialize;

use crate::analyzer_legacy::{LegacyConjugation, LegacyGloss, LegacyOrdinal, LegacySense};
use crate::dto::Utf16Text;
use crate::error::{ErrorCode, KernelError, Result};
use crate::romanization::strip_hints;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenDetails {
    pub text: Utf16Text,
    pub reading: Utf16Text,
    pub meanings: Vec<TokenMeaning>,
    pub components: Vec<TokenDetails>,
    pub conjugations: Vec<TokenConjugation>,
    pub alternatives: Vec<TokenDetails>,
    pub suffix_id: Option<String>,
    pub counter: Option<TokenCounter>,
    pub entity_kind: Option<TokenEntityKind>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum TokenEntityKind {
    ProperNoun,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenMeaning {
    pub gloss: String,
    pub pos: Vec<String>,
    pub fields: Vec<String>,
    pub info: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenConjugation {
    pub root: Option<TokenDetailForm>,
    pub properties: Vec<TokenConjugationProperty>,
    pub meanings: Vec<TokenMeaning>,
    pub via: Vec<TokenConjugation>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenDetailForm {
    pub text: String,
    pub reading: String,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenConjugationProperty {
    pub pos: String,
    #[serde(rename = "type")]
    pub kind: u8,
    pub negative: bool,
    pub formal: bool,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenCounter {
    pub value: String,
    pub ordinal: bool,
}

fn public_reading(value: Utf16Text) -> Utf16Text {
    Utf16Text::from_units(&strip_hints(value.units()))
}

fn public_string_reading(mut value: String) -> String {
    value.retain(|character| !matches!(character, '\u{200b}' | '\u{200c}'));
    value
}

fn public_counter_value(value: String) -> String {
    value
        .strip_prefix("Value: ")
        .map_or_else(|| value.clone(), str::to_owned)
}

pub(crate) fn token_details(value: LegacyGloss, entity: bool) -> Result<TokenDetails> {
    let text = value.text.ok_or_else(|| {
        KernelError::new(
            ErrorCode::Internal,
            "token details are missing display text",
        )
    })?;
    let reading = public_reading(value.kana.ok_or_else(|| {
        KernelError::new(ErrorCode::Internal, "token details are missing a reading")
    })?);
    let components = value
        .components
        .unwrap_or_default()
        .into_iter()
        .map(|component| token_details(component, false))
        .collect::<Result<Vec<_>>>()?;
    let conjugations = value
        .conj
        .unwrap_or_default()
        .into_iter()
        .map(token_conjugation)
        .collect();
    let counter = value.counter.map(|counter| TokenCounter {
        value: public_counter_value(counter.value),
        ordinal: matches!(counter.ordinal, LegacyOrdinal::Yes(true)),
    });
    Ok(TokenDetails {
        text,
        reading,
        meanings: value
            .gloss
            .unwrap_or_default()
            .into_iter()
            .filter(|meaning| {
                !(entity
                    && meaning.pos == "[n-pr]"
                    && meaning.gloss == "proper noun (named entity)")
            })
            .map(token_meaning)
            .collect(),
        components,
        conjugations,
        alternatives: Vec::new(),
        suffix_id: value.suffix_id,
        counter,
        entity_kind: entity.then_some(TokenEntityKind::ProperNoun),
    })
}

fn token_conjugation(value: LegacyConjugation) -> TokenConjugation {
    TokenConjugation {
        root: value.root.map(|root| TokenDetailForm {
            text: root.form,
            reading: public_string_reading(root.reading),
        }),
        properties: value
            .prop
            .into_iter()
            .map(|property| TokenConjugationProperty {
                pos: property.pos,
                kind: property.kind_id,
                negative: property.neg == Some(true),
                formal: property.fml == Some(true),
            })
            .collect(),
        meanings: value
            .gloss
            .unwrap_or_default()
            .into_iter()
            .map(token_meaning)
            .collect(),
        via: value
            .via
            .unwrap_or_default()
            .into_iter()
            .map(token_conjugation)
            .collect(),
    }
}

fn token_meaning(value: LegacySense) -> TokenMeaning {
    TokenMeaning {
        gloss: value.gloss,
        pos: canonical_delimited_values(&value.pos, '[', ']'),
        fields: value
            .field
            .as_deref()
            .map(|field| delimited_values(field, '{', '}'))
            .unwrap_or_default(),
        info: value.info,
    }
}

fn canonical_delimited_values(value: &str, open: char, close: char) -> Vec<String> {
    let mut values = delimited_values(value, open, close);
    values.sort();
    values.dedup();
    values
}

fn delimited_values(value: &str, open: char, close: char) -> Vec<String> {
    value
        .strip_prefix(open)
        .and_then(|value| value.strip_suffix(close))
        .unwrap_or(value)
        .split(',')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_owned)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::{
        canonical_delimited_values, delimited_values, public_counter_value, public_reading,
        public_string_reading,
    };
    use crate::dto::Utf16Text;

    #[test]
    fn parses_legacy_tag_lists_without_exposing_punctuation() {
        assert_eq!(delimited_values("[n, vt]", '[', ']'), ["n", "vt"]);
        assert_eq!(delimited_values("{comp,math}", '{', '}'), ["comp", "math"]);
        assert!(delimited_values("[]", '[', ']').is_empty());
        assert_eq!(
            canonical_delimited_values("[vt,v1,vt]", '[', ']'),
            ["v1", "vt"]
        );
    }

    #[test]
    fn removes_internal_reading_hints_and_counter_labels() {
        assert_eq!(
            public_reading(Utf16Text::from("\u{200c}は")).units(),
            "は".encode_utf16().collect::<Vec<_>>()
        );
        assert_eq!(public_string_reading("か\u{200b}く".to_owned()), "かく");
        assert_eq!(public_counter_value("Value: 3".to_owned()), "3");
        assert_eq!(public_counter_value("three".to_owned()), "three");
    }
}
