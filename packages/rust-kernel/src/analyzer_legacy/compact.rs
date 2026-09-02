use serde_json::json;

use super::{
    LegacyChunk, LegacyCompactResult, LegacyOptions, LegacyPath, LegacyReading, LegacySequence,
    LegacyToken, LegacyWordInfo, LegacyWordType, token_romanized,
};
use crate::dto::{
    AnalysisAlternative, AnalysisChunk, AnalysisComponent, AnalysisResult, AnalysisToken,
    PublicRoute,
};

pub(super) fn serialize(
    result: &AnalysisResult,
    options: &LegacyOptions<'_>,
) -> LegacyCompactResult {
    result
        .chunks
        .iter()
        .map(|chunk| match chunk {
            AnalysisChunk::Misc { text, .. } => LegacyChunk::Misc(text.clone()),
            AnalysisChunk::Word { start, paths, .. } => LegacyChunk::Paths(
                paths
                    .iter()
                    .map(|path| {
                        let words = path
                            .tokens
                            .iter()
                            .map(|token| {
                                let romanized = token_romanized(token, options.method);
                                let property = options.word_property.map_or_else(
                                    || json!([]),
                                    |property| property(romanized.units(), token),
                                );
                                LegacyToken(romanized, compact_word(token, *start), property)
                            })
                            .collect();
                        LegacyPath(words, path.score)
                    })
                    .collect(),
            ),
        })
        .collect()
}

fn compact_word(token: &AnalysisToken, chunk_start: usize) -> LegacyWordInfo {
    let start = token.start - chunk_start;
    let end = token.end - chunk_start;
    if token.alternatives.len() > 1 {
        let contextual = token
            .legacy
            .as_ref()
            .is_some_and(|facts| facts.contextual_reading);
        let readings = if contextual {
            LegacyReading::One(token.reading.clone())
        } else {
            let mut values = Vec::new();
            for alternative in &token.alternatives {
                if !values.contains(&alternative.reading) {
                    values.push(alternative.reading.clone());
                }
            }
            LegacyReading::Many(values)
        };
        return LegacyWordInfo {
            kind: word_type(token.route),
            text: token.text.clone(),
            truetext: None,
            kana: readings,
            seq: Some(LegacySequence::Many(
                token
                    .alternatives
                    .iter()
                    .filter_map(|value| value.root.as_ref().map(|root| root.seq))
                    .collect(),
            )),
            conjugations: None,
            score: token.score,
            components: Some(
                token
                    .alternatives
                    .iter()
                    .map(|value| alternative_word(value, start, end))
                    .collect(),
            ),
            alternative: Some(true),
            primary: None,
            start: Some(start),
            end: Some(end),
            counter: None,
            skipped: token.skipped,
            is_entity: None,
        };
    }
    LegacyWordInfo {
        kind: word_type(token.route),
        text: token.text.clone(),
        truetext: Some(
            token
                .true_text
                .clone()
                .unwrap_or_else(|| token.text.clone()),
        ),
        kana: LegacyReading::One(token.reading.clone()),
        seq: token
            .root
            .as_ref()
            .map(|root| LegacySequence::One(root.seq)),
        conjugations: (!token.inflection.is_empty()).then(|| token.inflection.clone()),
        score: token.score,
        components: (!token.components.is_empty())
            .then(|| token.components.iter().map(component_word).collect()),
        alternative: None,
        primary: None,
        start: Some(start),
        end: Some(end),
        counter: token.counter.clone(),
        skipped: token.skipped,
        is_entity: token.entity.then_some(true),
    }
}

fn component_word(component: &AnalysisComponent) -> LegacyWordInfo {
    LegacyWordInfo {
        kind: word_type(component.route.into()),
        text: component.text.clone(),
        truetext: Some(
            component
                .true_text
                .clone()
                .unwrap_or_else(|| component.text.clone()),
        ),
        kana: LegacyReading::One(component.reading.clone()),
        seq: component
            .root
            .as_ref()
            .map(|root| LegacySequence::One(root.seq)),
        conjugations: (!component.inflection.is_empty()).then(|| component.inflection.clone()),
        score: 0.0,
        components: None,
        alternative: None,
        primary: Some(component.primary),
        start: None,
        end: None,
        counter: None,
        skipped: 0,
        is_entity: None,
    }
}

fn alternative_word(alternative: &AnalysisAlternative, start: usize, end: usize) -> LegacyWordInfo {
    LegacyWordInfo {
        kind: word_type(alternative.route.into()),
        text: alternative.text.clone(),
        truetext: Some(
            alternative
                .true_text
                .clone()
                .unwrap_or_else(|| alternative.text.clone()),
        ),
        kana: LegacyReading::One(alternative.reading.clone()),
        seq: alternative
            .root
            .as_ref()
            .map(|root| LegacySequence::One(root.seq)),
        conjugations: (!alternative.inflection.is_empty()).then(|| alternative.inflection.clone()),
        score: alternative.score,
        components: (!alternative.components.is_empty())
            .then(|| alternative.components.iter().map(component_word).collect()),
        alternative: None,
        primary: None,
        start: Some(start),
        end: Some(end),
        counter: alternative.counter.clone(),
        skipped: 0,
        is_entity: None,
    }
}

fn word_type(route: PublicRoute) -> LegacyWordType {
    match route {
        PublicRoute::Kana => LegacyWordType::Kana,
        PublicRoute::Kanji => LegacyWordType::Kanji,
        PublicRoute::Gap => LegacyWordType::Gap,
    }
}
