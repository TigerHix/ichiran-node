use std::collections::BTreeSet;

use super::super::*;
use crate::support::{SupportSplitKind, SupportSplitPart, SupportSplitWord};

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct SegmentSplit {
    pub(crate) candidate: MaterializedCandidate,
    pub(crate) added_score: i32,
}

impl AnalyzerLexicon<'_> {
    pub fn score_split(
        &mut self,
        definition_seq: u32,
        route: Route,
        surface: &[u16],
    ) -> Result<Option<ScoreSplit>> {
        let key = SplitKey {
            definition_seq,
            route,
            surface: surface.to_vec(),
        };
        if let Some(cached) = self.score_split_cache.get(&key) {
            return Ok(cached.clone());
        }
        if !self.score_split_in_progress.insert(key.clone()) {
            return Ok(None);
        }
        let result = self.resolve_score_split(definition_seq, route, surface);
        self.score_split_in_progress.remove(&key);
        let result = result?;
        self.score_split_cache.insert(key, result.clone());
        Ok(result)
    }

    fn resolve_score_split(
        &mut self,
        definition_seq: u32,
        route: Route,
        surface: &[u16],
    ) -> Result<Option<ScoreSplit>> {
        let surface_text = utf16_string(surface, "score split surface")?;
        let Some(split) = self.annotations.split(
            definition_seq,
            route,
            &surface_text,
            SupportSplitKind::Split,
        )?
        else {
            return Ok(None);
        };
        if split.parts.contains(&SupportSplitPart::Score) {
            return Ok(Some(ScoreSplit::Add(split.score)));
        }
        if split.parts.contains(&SupportSplitPart::Pscore) {
            return Ok(Some(ScoreSplit::Proportional(split.score)));
        }
        let mut parts = Vec::with_capacity(split.parts.len());
        for part in &split.parts {
            let SupportSplitPart::Word(part) = part else {
                return Ok(None);
            };
            let Some(candidate) = self.split_part(part)? else {
                return Ok(None);
            };
            parts.push(candidate.score_facts);
        }
        if parts.is_empty() {
            return Ok(None);
        }
        Ok(Some(ScoreSplit::Parts {
            score: split.score,
            parts,
            truncated_last: None,
        }))
    }

    pub fn segment_split(
        &mut self,
        candidate: &MaterializedCandidate,
    ) -> Result<Option<SegmentSplit>> {
        if candidate.kind != CandidateKind::Simple {
            return Ok(None);
        }
        let Some(definition_seq) = candidate.definition_seq else {
            return Ok(None);
        };
        let true_text = utf16_string(&candidate.true_text, "segment split surface")?;
        let Some(split) = self.annotations.split(
            definition_seq,
            candidate.route,
            &true_text,
            SupportSplitKind::Segsplit,
        )?
        else {
            return Ok(None);
        };
        let mut values = Vec::with_capacity(split.parts.len());
        for part in &split.parts {
            let SupportSplitPart::Word(part) = part else {
                return Ok(None);
            };
            let Some(value) = self.split_part(part)? else {
                return Ok(None);
            };
            values.push(value);
        }
        let Some(primary) = values
            .get(usize::from(split.primary))
            .or_else(|| values.first())
            .cloned()
        else {
            return Ok(None);
        };
        let components = values
            .iter()
            .map(|value| value.as_component(value.physical_key == primary.physical_key))
            .collect();
        let mut text = Vec::new();
        let mut reading = Vec::new();
        let connector = utf16(&split.connector);
        for (index, value) in values.iter().enumerate() {
            text.extend_from_slice(&value.text);
            if index > 0 {
                reading.extend_from_slice(&connector);
            }
            reading.extend_from_slice(&value.reading);
        }
        let last = values.last();
        Ok(Some(SegmentSplit {
            added_score: split.score,
            candidate: MaterializedCandidate {
                kind: CandidateKind::Compound,
                text,
                true_text: candidate.true_text.clone(),
                route: candidate.route,
                reading,
                public_seq: primary.public_seq,
                physical_seq: primary.physical_seq,
                physical_key: primary.physical_key.clone(),
                physical_group: primary.physical_group,
                lookup_locators: Vec::new(),
                member_ord: primary.member_ord,
                entry_index: primary.entry_index,
                root: primary.root.clone(),
                inflection: last.map_or_else(Vec::new, |value| value.inflection.clone()),
                score_facts: primary.score_facts.clone(),
                components,
                counter: None,
                suffix_class: last.and_then(|value| value.suffix_class.clone()),
                definition_seq: candidate.definition_seq,
                semantic_members: primary.semantic_members.clone(),
                identity_roots: primary.identity_roots.clone(),
                conjugation_selection: ConjugationSelection::Default,
            },
        }))
    }

    fn split_part(&mut self, part: &SupportSplitWord) -> Result<Option<MaterializedCandidate>> {
        let values = self.lexical(&utf16(&part.text))?;
        if let Some(exact) = values.iter().find(|value| {
            value.route == part.route
                && (value.public_seq == Some(part.seq)
                    || value.physical_seq == Some(i64::from(part.seq)))
        }) {
            return Ok(Some(exact.clone()));
        }
        let Some(generated) = part.generated.as_ref().filter(|values| !values.is_empty()) else {
            return Ok(None);
        };
        let wanted: BTreeSet<_> = generated
            .iter()
            .map(|value| SplitSignature {
                from: value.from,
                via: value.via,
                pos: value.pos.clone(),
                kind: value.kind,
                negative: value.negative,
                formal: value.formal,
            })
            .collect();
        Ok(values.into_iter().find(|value| {
            value.route == part.route && semantic_signature(&value.semantic_members) == wanted
        }))
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct SplitSignature {
    from: u32,
    via: bool,
    pos: String,
    kind: u16,
    negative: Option<bool>,
    formal: Option<bool>,
}

fn semantic_signature(members: &[SemanticMember]) -> BTreeSet<SplitSignature> {
    members
        .iter()
        .filter_map(|member| {
            let property = member.inflection.last()?;
            let from = member
                .root
                .as_ref()
                .map(|root| root.seq)
                .or(member.public_seq)?;
            Some(SplitSignature {
                from,
                via: member.inflection.len() > 1,
                pos: property.pos.clone(),
                kind: u16::from(property.kind),
                negative: property.negative,
                formal: property.formal,
            })
        })
        .collect()
}
