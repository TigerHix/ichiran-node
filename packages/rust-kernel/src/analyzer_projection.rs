use std::collections::HashMap;

use crate::analyzer_lexicon::{
    AnalysisInflection, AnalysisRoot as LexiconRoot, CandidateComponent, CandidateKind,
    ConjugationSelection, MaterializedCandidate, SemanticMember, StageKey,
};
use crate::analyzer_model::{EntityHint, PathPart, PathResult, ScoreInfo, Segment, SegmentGroup};
use crate::analyzer_scoring::select_alternatives;
use crate::characters::{CharClass, character_class, test_word};
use crate::dto::{
    AnalysisAlternative, AnalysisComponent, AnalysisPath, AnalysisRoot, AnalysisToken,
    LegacyConjugationSelection, LegacyPresentationFacts, LegacySemanticMember, PublicRoute,
    Utf16Text,
};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::MorphologyProperty;
use crate::romanization::{RomanizationName, romanize_word};
use crate::text::string as utf16_string;

#[derive(Clone, Debug, PartialEq)]
pub struct ProjectionScoredCandidate {
    pub candidate: MaterializedCandidate,
    pub info: ScoreInfo,
}

pub fn project_paths(
    text: &[u16],
    paths: &[PathResult],
    candidates: &HashMap<i64, ProjectionScoredCandidate>,
    entities: &[EntityHint],
) -> Result<Vec<AnalysisPath>> {
    paths
        .iter()
        .map(|path| project_path(text, path, candidates, entities))
        .collect()
}

pub fn project_path(
    text: &[u16],
    path: &PathResult,
    candidates: &HashMap<i64, ProjectionScoredCandidate>,
    entities: &[EntityHint],
) -> Result<AnalysisPath> {
    Ok(AnalysisPath {
        score: path.score,
        tokens: project_tokens(text, &path.parts, candidates, entities)?,
    })
}

pub fn project_tokens(
    text: &[u16],
    parts: &[PathPart],
    candidates: &HashMap<i64, ProjectionScoredCandidate>,
    entities: &[EntityHint],
) -> Result<Vec<AnalysisToken>> {
    let mut tokens = Vec::new();
    let mut offset = 0;
    for group in parts.iter().filter_map(|part| match part {
        PathPart::Group(group) => Some(group),
        PathPart::Adjustment(_) => None,
    }) {
        if group.start > offset {
            tokens.push(gap(text, offset, group.start)?);
        }
        let Some(first) = group.segments.first() else {
            continue;
        };
        if first.entity {
            tokens.push(entity_token(text, group, first)?);
            offset = group.end;
            continue;
        }
        let retained = select_alternatives(&group.segments);
        let primary = proxy_primary(first, &retained, candidates);
        let Some(scored) = candidates.get(&primary.candidate_id) else {
            continue;
        };
        let alternatives = retained
            .iter()
            .filter_map(|segment| {
                candidates
                    .get(&segment.candidate_id)
                    .map(|value| (segment, value))
            })
            .map(|(segment, value)| alternative(segment, value))
            .collect::<Result<Vec<_>>>()?;
        let skipped = group
            .matches
            .checked_sub(alternatives.len())
            .ok_or_else(|| {
                internal("analyzer group retained more alternatives than its match count")
            })?;
        let candidate = &scored.candidate;
        tokens.push(AnalysisToken {
            candidate_id: Some(primary.candidate_id),
            start: group.start,
            end: group.end,
            text: Utf16Text::from_units(&candidate.text),
            true_text: true_text(&candidate.text, &candidate.true_text),
            route: candidate.route.into(),
            reading: Utf16Text::from_units(&candidate.reading),
            romanized: Utf16Text::from_units(&romanized(&candidate.reading, Some(&candidate.text))),
            pos: scored.info.positions.clone(),
            score: primary.score,
            entry_index: candidate.entry_index,
            root: candidate.root.as_ref().map(public_root).transpose()?,
            inflection: candidate.inflection.iter().map(public_inflection).collect(),
            components: candidate
                .components
                .iter()
                .map(public_component)
                .collect::<Result<Vec<_>>>()?,
            alternatives,
            skipped,
            entity: entities
                .iter()
                .any(|entity| entity.start == group.start && entity.end == group.end),
            counter: candidate.counter.clone(),
            legacy: Some(candidate_facts(candidate)?),
        });
        offset = group.end;
    }
    if offset < text.len() {
        tokens.push(gap(text, offset, text.len())?);
    }
    fix_nani(&mut tokens);
    Ok(tokens)
}

pub fn shift_token(mut token: AnalysisToken, offset: usize) -> Result<AnalysisToken> {
    if offset == 0 {
        return Ok(token);
    }
    token.start = token
        .start
        .checked_add(offset)
        .ok_or_else(|| internal("shifted analyzer token start overflows"))?;
    token.end = token
        .end
        .checked_add(offset)
        .ok_or_else(|| internal("shifted analyzer token end overflows"))?;
    Ok(token)
}

pub fn gap(text: &[u16], start: usize, end: usize) -> Result<AnalysisToken> {
    let value = span(text, start, end, "analyzer gap")?;
    Ok(AnalysisToken {
        candidate_id: None,
        start,
        end,
        text: Utf16Text::from_units(value),
        true_text: None,
        route: PublicRoute::Gap,
        reading: Utf16Text::from_units(value),
        romanized: Utf16Text::from_units(&romanized(value, None)),
        pos: Vec::new(),
        score: 0.0,
        entry_index: None,
        root: None,
        inflection: Vec::new(),
        components: Vec::new(),
        alternatives: Vec::new(),
        skipped: 0,
        entity: false,
        counter: None,
        legacy: None,
    })
}

fn entity_token(text: &[u16], group: &SegmentGroup, first: &Segment) -> Result<AnalysisToken> {
    let value = span(text, group.start, group.end, "analyzer entity")?;
    Ok(AnalysisToken {
        candidate_id: Some(first.candidate_id),
        start: group.start,
        end: group.end,
        text: Utf16Text::from_units(value),
        true_text: None,
        route: if test_word(value, CharClass::Kana) {
            PublicRoute::Kana
        } else {
            PublicRoute::Kanji
        },
        reading: Utf16Text::from_units(value),
        romanized: Utf16Text::from_units(&romanized(value, None)),
        pos: vec!["proper-noun".to_owned()],
        score: first.score,
        entry_index: None,
        root: None,
        inflection: Vec::new(),
        components: Vec::new(),
        alternatives: Vec::new(),
        skipped: 0,
        entity: true,
        counter: None,
        legacy: None,
    })
}

fn proxy_primary<'a>(
    first: &'a Segment,
    retained: &'a [Segment],
    candidates: &HashMap<i64, ProjectionScoredCandidate>,
) -> &'a Segment {
    if candidates
        .get(&first.candidate_id)
        .is_none_or(|value| value.candidate.kind != CandidateKind::Proxy)
    {
        return first;
    }
    retained
        .iter()
        .filter(|segment| segment.score == first.score)
        .filter(|segment| {
            candidates
                .get(&segment.candidate_id)
                .is_some_and(|value| value.candidate.kind == CandidateKind::Proxy)
        })
        .min_by_key(|segment| {
            candidates[&segment.candidate_id]
                .candidate
                .public_seq
                .unwrap_or(u32::MAX)
        })
        .unwrap_or(first)
}

fn alternative(
    segment: &Segment,
    scored: &ProjectionScoredCandidate,
) -> Result<AnalysisAlternative> {
    let candidate = &scored.candidate;
    Ok(AnalysisAlternative {
        candidate_id: segment.candidate_id,
        text: Utf16Text::from_units(&candidate.text),
        true_text: true_text(&candidate.text, &candidate.true_text),
        route: candidate.route,
        reading: Utf16Text::from_units(&candidate.reading),
        romanized: Utf16Text::from_units(&romanized(&candidate.reading, Some(&candidate.text))),
        pos: scored.info.positions.clone(),
        score: segment.score,
        entry_index: candidate.entry_index,
        root: candidate.root.as_ref().map(public_root).transpose()?,
        inflection: candidate.inflection.iter().map(public_inflection).collect(),
        components: candidate
            .components
            .iter()
            .map(public_component)
            .collect::<Result<Vec<_>>>()?,
        counter: candidate.counter.clone(),
        legacy: Some(candidate_facts(candidate)?),
    })
}

fn public_component(component: &CandidateComponent) -> Result<AnalysisComponent> {
    Ok(AnalysisComponent {
        text: Utf16Text::from_units(&component.text),
        true_text: component.true_text.as_deref().map(Utf16Text::from_units),
        route: component.route,
        reading: Utf16Text::from_units(&component.reading),
        entry_index: component.entry_index,
        root: component.root.as_ref().map(public_root).transpose()?,
        inflection: component.inflection.iter().map(public_inflection).collect(),
        primary: component.primary,
        legacy: Some(component_facts(component)?),
    })
}

fn candidate_facts(candidate: &MaterializedCandidate) -> Result<LegacyPresentationFacts> {
    presentation_facts(
        candidate.physical_group,
        candidate.suffix_class.clone(),
        candidate.definition_seq,
        &candidate.semantic_members,
        candidate.identity_roots.clone(),
        candidate.conjugation_selection,
    )
}

fn component_facts(component: &CandidateComponent) -> Result<LegacyPresentationFacts> {
    presentation_facts(
        component.physical_group,
        component.suffix_class.clone(),
        component.definition_seq,
        &component.semantic_members,
        component.identity_roots.clone(),
        component.conjugation_selection,
    )
}

fn presentation_facts(
    physical_group: Option<u32>,
    suffix_class: Option<String>,
    definition_seq: Option<u32>,
    members: &[SemanticMember],
    identity_roots: Vec<u32>,
    selection: ConjugationSelection,
) -> Result<LegacyPresentationFacts> {
    Ok(LegacyPresentationFacts {
        physical_group,
        suffix_class,
        definition_seq,
        semantic_members: members
            .iter()
            .map(|member| {
                Ok(LegacySemanticMember {
                    entry_index: member.entry_index,
                    root: member.root.as_ref().map(public_root).transpose()?,
                    inflection: member.inflection.iter().map(public_inflection).collect(),
                    stage_groups: member.stage_groups.clone(),
                    stage_keys: member.stage_keys.iter().map(stage_key).collect(),
                    stage_member_ords: member.stage_member_ords.clone(),
                    stage_prop_ords: member.stage_prop_ords.clone(),
                    member_ord: member.member_ord,
                })
            })
            .collect::<Result<Vec<_>>>()?,
        identity_roots,
        conjugation_selection: match selection {
            ConjugationSelection::Default => LegacyConjugationSelection::Default,
            ConjugationSelection::Explicit => LegacyConjugationSelection::Explicit,
            ConjugationSelection::Root => LegacyConjugationSelection::Root,
        },
        contextual_reading: false,
    })
}

fn stage_key(value: &Option<StageKey>) -> Option<String> {
    value.as_ref().map(|stage| {
        format!(
            "{}:{}",
            stage.root_seq,
            stage
                .aliases
                .iter()
                .map(u16::to_string)
                .collect::<Vec<_>>()
                .join(",")
        )
    })
}

fn public_root(root: &LexiconRoot) -> Result<AnalysisRoot> {
    Ok(AnalysisRoot {
        seq: root.seq,
        form: utf16_string(&root.form, "analyzer root form")?,
        reading: utf16_string(&root.reading, "analyzer root reading")?,
    })
}

fn public_inflection(value: &AnalysisInflection) -> MorphologyProperty {
    MorphologyProperty {
        pos: value.pos.clone(),
        kind: value.kind,
        negative: value.negative,
        formal: value.formal,
        ordinal: value.ordinal,
    }
}

fn true_text(text: &[u16], true_text: &[u16]) -> Option<Utf16Text> {
    (text != true_text).then(|| Utf16Text::from_units(true_text))
}

fn romanized(reading: &[u16], spelling: Option<&[u16]>) -> Vec<u16> {
    romanize_word(
        reading,
        RomanizationName::HepburnTraditional,
        spelling,
        true,
    )
}

fn fix_nani(tokens: &mut [AnalysisToken]) {
    const NAN_CLASSES: &[&str] = &[
        "ba", "bi", "bu", "be", "bo", "pa", "pi", "pu", "pe", "po", "da", "dji", "dzu", "de", "do",
        "za", "ji", "zu", "ze", "zo", "ta", "chi", "tsu", "te", "to", "na", "nu", "ne", "no", "ra",
        "ri", "ru", "re", "ro",
    ];
    for index in 0..tokens.len().saturating_sub(1) {
        if tokens[index].text.units() != [0x4f55] {
            continue;
        }
        let next = &tokens[index + 1];
        let mut nan = false;
        let mut nani = false;
        for reading in std::iter::once(&next.reading).chain(
            next.alternatives
                .iter()
                .map(|alternative| &alternative.reading),
        ) {
            let Some(first) = reading.units().first() else {
                continue;
            };
            if character_class(*first).is_some_and(|class| NAN_CLASSES.contains(&class)) {
                nan = true;
            } else {
                nani = true;
            }
        }
        let replacement = if nani {
            Some("なに")
        } else if nan {
            Some("なん")
        } else {
            None
        };
        if let Some(reading) = replacement {
            let reading: Vec<u16> = reading.encode_utf16().collect();
            tokens[index].reading = Utf16Text::from_units(&reading);
            tokens[index].romanized = Utf16Text::from_units(&romanized(&reading, None));
            if let Some(facts) = tokens[index].legacy.as_mut() {
                facts.contextual_reading = true;
            }
        }
    }
}

fn span<'a>(text: &'a [u16], start: usize, end: usize, label: &str) -> Result<&'a [u16]> {
    text.get(start..end).ok_or_else(|| {
        KernelError::new(
            ErrorCode::OutOfRange,
            format!("{label} span {start}..{end} lies outside the input"),
        )
    })
}

fn internal(message: impl Into<String>) -> KernelError {
    KernelError::new(ErrorCode::Internal, message)
}

#[cfg(test)]
mod tests;
