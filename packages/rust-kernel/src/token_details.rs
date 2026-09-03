use serde::Serialize;

use crate::analyzer_legacy::{LegacyConjugation, LegacyGloss, LegacyOrdinal, LegacySense};
use crate::dto::Utf16Text;
use crate::error::{ErrorCode, KernelError, Result};

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct TokenDetails {
    pub text: Utf16Text,
    pub reading: Utf16Text,
    pub meanings: Vec<TokenMeaning>,
    pub components: Vec<TokenDetails>,
    pub conjugations: Vec<TokenConjugation>,
    pub alternatives: Vec<TokenDetails>,
    pub suffix: Option<String>,
    pub counter: Option<TokenCounter>,
    pub entity: bool,
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

pub(crate) fn token_details(value: LegacyGloss, entity: bool) -> Result<TokenDetails> {
    let text = value.text.ok_or_else(|| {
        KernelError::new(
            ErrorCode::Internal,
            "token details are missing display text",
        )
    })?;
    let reading = value.kana.ok_or_else(|| {
        KernelError::new(ErrorCode::Internal, "token details are missing a reading")
    })?;
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
        value: counter.value,
        ordinal: matches!(counter.ordinal, LegacyOrdinal::Yes(true)),
    });
    Ok(TokenDetails {
        text,
        reading,
        meanings: value
            .gloss
            .unwrap_or_default()
            .into_iter()
            .map(token_meaning)
            .collect(),
        components,
        conjugations,
        alternatives: Vec::new(),
        suffix: value.suffix,
        counter,
        entity,
    })
}

fn token_conjugation(value: LegacyConjugation) -> TokenConjugation {
    TokenConjugation {
        root: value.root.map(|root| TokenDetailForm {
            text: root.form,
            reading: root.reading,
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
        pos: delimited_values(&value.pos, '[', ']'),
        fields: value
            .field
            .as_deref()
            .map(|field| delimited_values(field, '{', '}'))
            .unwrap_or_default(),
        info: value.info,
    }
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
    use super::delimited_values;

    #[test]
    fn parses_legacy_tag_lists_without_exposing_punctuation() {
        assert_eq!(delimited_values("[n, vt]", '[', ']'), ["n", "vt"]);
        assert_eq!(delimited_values("{comp,math}", '{', '}'), ["comp", "math"]);
        assert!(delimited_values("[]", '[', ']').is_empty());
    }
}
