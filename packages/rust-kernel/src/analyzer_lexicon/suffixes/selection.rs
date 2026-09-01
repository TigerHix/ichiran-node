use std::collections::HashSet;

use super::super::*;
use crate::analyzer_model::{ConjugationProperty, ScoreWordKind};
use crate::support::{SupportConjugation, SupportConjugations, SupportSuffixForm};

impl AnalyzerLexicon<'_> {
    pub(super) fn select_conjugations(
        &self,
        candidate: &MaterializedCandidate,
        predicate: impl Fn(&Conjugation) -> bool,
        allow_root: bool,
    ) -> Result<Option<MaterializedCandidate>> {
        let current = conjugations(&candidate.score_facts);
        if current.is_empty() {
            return Ok(allow_root.then(|| candidate.clone()));
        }
        if !current.iter().any(&predicate) {
            return Ok(None);
        }
        let ScoreCandidate::Word(word_facts) = &candidate.score_facts else {
            return Ok(Some(candidate.clone()));
        };
        if candidate.kind != CandidateKind::Simple {
            return Ok(Some(candidate.clone()));
        }

        let selected_rows: HashSet<_> = candidate
            .semantic_members
            .iter()
            .filter_map(|member| {
                let conjugation = member_conjugation(member, word_facts.seq)?;
                predicate(&conjugation).then(|| RowKey::from(member))
            })
            .collect();
        if selected_rows.is_empty() {
            return Ok(Some(candidate.clone()));
        }
        let semantic_members: Vec<_> = candidate
            .semantic_members
            .iter()
            .filter(|member| selected_rows.contains(&RowKey::from(*member)))
            .cloned()
            .collect();
        let mut seen = Vec::new();
        let selected_conjugations: Vec<_> = semantic_members
            .iter()
            .filter_map(|member| {
                let conjugation = member_conjugation(member, word_facts.seq)?;
                let key = (RowKey::from(member), conjugation.clone());
                if seen.contains(&key) {
                    None
                } else {
                    seen.push(key);
                    Some(conjugation)
                }
            })
            .collect();
        if selected_conjugations.is_empty() {
            return Ok(Some(candidate.clone()));
        }

        let selected = &semantic_members[0];
        let preserve_direct_target = candidate.inflection.is_empty();
        let target_entry = candidate
            .physical_seq
            .filter(|seq| *seq > 0)
            .map(|seq| self.roots.find_entry_index(seq as u32))
            .transpose()?
            .flatten();
        let secondary_only = selected_conjugations
            .iter()
            .all(|conjugation| conjugation.via.is_some());
        let scoring_members: Vec<_> = semantic_members
            .iter()
            .filter(|member| {
                if secondary_only {
                    member.inflection.len() > 1
                } else {
                    member.inflection.len() == 1
                }
            })
            .cloned()
            .collect();
        let lineage_facts =
            self.member_sequence_facts(&candidate.semantic_members, &scoring_members)?;
        let target_facts = word_facts.self_facts;
        let mut positions = target_entry
            .map(|entry| self.roots.entry_positions(entry))
            .transpose()?
            .unwrap_or_default();
        for member in &semantic_members {
            if let Some(entry) = member.entry_index {
                extend_unique(&mut positions, self.roots.entry_positions(entry)?);
            }
        }
        extend_unique(
            &mut positions,
            selected_conjugations
                .iter()
                .map(|value| value.property.pos.clone()),
        );
        let mut score_facts = word_facts.clone();
        score_facts.conjugation_only = true;
        score_facts.conjugations = selected_conjugations;
        score_facts.positions = positions;
        score_facts.lineage = SequenceFacts {
            all_archived: target_facts.all_archived
                && !lineage_facts.is_empty()
                && lineage_facts.iter().all(|value| value.all_archived),
            prefer_kana: target_facts.prefer_kana
                || lineage_facts.iter().any(|value| value.prefer_kana),
            prefer_kana_on_ordinal_zero: target_facts.prefer_kana_on_ordinal_zero
                || lineage_facts
                    .iter()
                    .any(|value| value.prefer_kana_on_ordinal_zero),
        };
        if !preserve_direct_target
            && let (Some(entry), Some(n_kanji)) =
                (score_facts.entry.as_mut(), selected.target_n_kanji)
        {
            entry.n_kanji = u32::from(n_kanji);
        }

        let mut result = candidate.clone();
        if !preserve_direct_target {
            result.public_seq = selected.public_seq;
            result.entry_index = selected.entry_index;
            result.root.clone_from(&selected.root);
            result.inflection.clone_from(&selected.inflection);
            result.member_ord = selected.member_ord;
        }
        result.score_facts = ScoreCandidate::Word(score_facts);
        result.semantic_members = semantic_members;
        if preserve_direct_target && let Some(seq) = candidate.public_seq {
            result.identity_roots = vec![seq];
        }
        result.conjugation_selection = ConjugationSelection::Explicit;
        Ok(Some(result))
    }

    pub(super) fn suffix_component(
        &mut self,
        form: &SupportSuffixForm,
    ) -> Result<Option<MaterializedCandidate>> {
        let text = utf16(&form.text);
        let values = self.lexical(&text)?;
        let explicit = match &form.conjugations {
            Some(SupportConjugations::Values(values)) => values.as_slice(),
            _ => &[],
        };
        let root_seqs: HashSet<_> = explicit.iter().map(|value| value.from).collect();
        let explicitly_selected = (!explicit.is_empty())
            .then(|| {
                values.iter().find(|candidate| {
                    candidate
                        .semantic_members
                        .iter()
                        .any(|member| explicit.iter().any(|row| selected_by(member, row)))
                })
            })
            .flatten();
        if !explicit.is_empty() && explicitly_selected.is_none() {
            return Err(internal(format!(
                "Explicit suffix member is unavailable for {:?}",
                form.text
            )));
        }
        let candidate = explicitly_selected
            .or_else(|| {
                values
                    .iter()
                    .find(|candidate| candidate.physical_seq == Some(i64::from(form.seq)))
            })
            .or_else(|| {
                values.iter().find(|candidate| {
                    candidate
                        .public_seq
                        .is_some_and(|seq| root_seqs.contains(&seq))
                })
            })
            .or_else(|| {
                values
                    .iter()
                    .find(|candidate| candidate.public_seq == Some(form.seq))
            })
            .or_else(|| values.first());
        let Some(candidate) = candidate else {
            return Ok(None);
        };
        let candidate_word = match &candidate.score_facts {
            ScoreCandidate::Word(facts) => facts,
            ScoreCandidate::Compound(_) => return Ok(Some(candidate.clone())),
        };

        let selection = match &form.conjugations {
            Some(SupportConjugations::Root) => ConjugationSelection::Root,
            None => ConjugationSelection::Default,
            Some(SupportConjugations::Values(_)) => ConjugationSelection::Explicit,
        };
        let semantic_members = match selection {
            ConjugationSelection::Root => Vec::new(),
            ConjugationSelection::Default => candidate.semantic_members.clone(),
            ConjugationSelection::Explicit => candidate
                .semantic_members
                .iter()
                .filter(|member| explicit.iter().any(|row| selected_by(member, row)))
                .cloned()
                .collect(),
        };
        let selected_member = semantic_members.first();
        let root_seq = selected_member
            .and_then(|member| member.root.as_ref().map(|root| root.seq))
            .or_else(|| selected_member.and_then(|member| member.public_seq))
            .or_else(|| explicit.first().map(|value| value.from))
            .or(candidate.public_seq)
            .unwrap_or(form.seq);
        let root_entry = selected_member
            .and_then(|member| member.entry_index)
            .map(Some)
            .unwrap_or(self.roots.find_entry_index(root_seq)?);
        let target_entry = self.roots.find_entry_index(form.seq)?;
        let lexical_target = target_entry.and_then(|entry| {
            values.iter().find(|value| {
                value.public_seq == Some(form.seq)
                    && value.entry_index == Some(entry)
                    && value.inflection.is_empty()
            })
        });
        let root_facts = sequence_facts(self.roots, root_entry)?;
        let self_facts = sequence_facts(self.roots, target_entry)?;
        let lineage = if target_entry.is_none() {
            root_facts
        } else {
            SequenceFacts {
                all_archived: self_facts.all_archived && root_facts.all_archived,
                prefer_kana: self_facts.prefer_kana || root_facts.prefer_kana,
                prefer_kana_on_ordinal_zero: self_facts.prefer_kana_on_ordinal_zero
                    || root_facts.prefer_kana_on_ordinal_zero,
            }
        };
        let conjugations = explicit
            .iter()
            .map(support_conjugation)
            .collect::<Result<Vec<_>>>()?;
        let mut positions = root_entry
            .map(|entry| self.roots.entry_positions(entry))
            .transpose()?
            .unwrap_or_default();
        if let Some(entry) = target_entry {
            extend_unique(&mut positions, self.roots.entry_positions(entry)?);
        }
        extend_unique(
            &mut positions,
            conjugations.iter().map(|value| value.property.pos.clone()),
        );
        let entry = target_entry
            .map(|entry| {
                Ok(EntryScoreFacts {
                    root: true,
                    n_kanji: u32::from(self.roots.entry_n_kanji(entry)?),
                    primary_nokanji: self.roots.entry_primary_nokanji(entry)?,
                })
            })
            .transpose()?
            .or(Some(EntryScoreFacts {
                root: false,
                n_kanji: 0,
                primary_nokanji: false,
            }));
        let physical_seq = if selection == ConjugationSelection::Root {
            root_seq
        } else {
            form.seq
        };
        let score_facts = ScoreCandidate::Word(WordScoreFacts {
            kind: ScoreWordKind::Word,
            text: text.clone(),
            true_text: text.clone(),
            true_text_follows_text: true,
            route: Route::Kana,
            seq: Some(i64::from(physical_seq)),
            ord: i32::from(form.ord),
            common: if selection == ConjugationSelection::Root {
                form.common.map(i32::from)
            } else {
                None
            },
            nokanji: form.nokanji,
            entry,
            conjugation_only: selection != ConjugationSelection::Root && !conjugations.is_empty(),
            conjugations,
            positions,
            self_facts,
            lineage,
            inherited_common: candidate_word.inherited_common,
            inherited_ord: candidate_word.inherited_ord,
            split: self.score_split(form.seq, Route::Kana, &text)?,
            suru_break: None,
        });

        let mut result = candidate.clone();
        result.kind = CandidateKind::Simple;
        result.text.clone_from(&text);
        result.true_text.clone_from(&text);
        result.route = Route::Kana;
        result.reading.clone_from(&text);
        result.public_seq = lexical_target
            .and_then(|value| value.public_seq)
            .or(Some(root_seq));
        result.physical_seq = Some(i64::from(physical_seq));
        result.physical_key = PhysicalKey::Sequence(physical_seq);
        result.member_ord = selected_member
            .and_then(|member| member.member_ord)
            .or(candidate.member_ord);
        result.entry_index = lexical_target
            .and_then(|value| value.entry_index)
            .or(root_entry);
        result.root = lexical_target
            .and_then(|value| value.root.clone())
            .or_else(|| selected_member.and_then(|member| member.root.clone()))
            .or_else(|| {
                candidate
                    .root
                    .as_ref()
                    .filter(|root| root.seq == root_seq)
                    .cloned()
            })
            .or_else(|| {
                Some(AnalysisRoot {
                    seq: root_seq,
                    form: candidate
                        .root
                        .as_ref()
                        .map(|root| root.form.clone())
                        .or_else(|| form.best_kanji.as_deref().map(utf16))
                        .unwrap_or_else(|| text.clone()),
                    reading: candidate
                        .root
                        .as_ref()
                        .map(|root| root.reading.clone())
                        .unwrap_or_else(|| text.clone()),
                })
            });
        result.inflection = lexical_target
            .map(|_| Vec::new())
            .or_else(|| selected_member.map(|member| member.inflection.clone()))
            .or_else(|| {
                candidate
                    .root
                    .as_ref()
                    .filter(|root| root.seq == root_seq)
                    .map(|_| candidate.inflection.clone())
            })
            .unwrap_or_default();
        result.score_facts = score_facts;
        result.suffix_class = self.support.suffix_class(form.seq)?;
        result.definition_seq = Some(form.seq);
        result.semantic_members = semantic_members;
        if lexical_target.is_some() {
            result.identity_roots = vec![form.seq];
        }
        result.conjugation_selection = selection;
        Ok(Some(result))
    }
}

fn conjugations(candidate: &ScoreCandidate) -> &[Conjugation] {
    match candidate {
        ScoreCandidate::Word(value) => &value.conjugations,
        ScoreCandidate::Compound(value) => &value.conjugations,
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum RowKey {
    Group(u32, u8),
    Semantic(Option<u32>, Vec<AnalysisInflection>),
}

impl From<&SemanticMember> for RowKey {
    fn from(member: &SemanticMember) -> Self {
        match (
            member.stage_groups.last().copied().flatten(),
            member.stage_member_ords.last().copied().flatten(),
        ) {
            (Some(group), Some(ordinal)) => Self::Group(group, ordinal),
            _ => Self::Semantic(member.public_seq, member.inflection.clone()),
        }
    }
}

fn member_conjugation(member: &SemanticMember, physical_seq: Option<i64>) -> Option<Conjugation> {
    let property = member.inflection.last()?;
    let from = member
        .root
        .as_ref()
        .map(|root| root.seq)
        .or(member.public_seq)?;
    Some(Conjugation {
        seq: physical_seq.unwrap_or(-i64::from(from)),
        from: i64::from(from),
        via: (member.inflection.len() > 1).then_some(-i64::from(from)),
        property: ConjugationProperty {
            pos: property.pos.clone(),
            kind: property.kind,
            negative: property.negative,
            formal: property.formal,
        },
    })
}

fn selected_by(member: &SemanticMember, conjugation: &SupportConjugation) -> bool {
    let Some(property) = member.inflection.last() else {
        return false;
    };
    let root_seq = member
        .root
        .as_ref()
        .map(|root| root.seq)
        .or(member.public_seq);
    root_seq == Some(conjugation.from)
        && member.inflection.len() == if conjugation.via.is_none() { 1 } else { 2 }
        && conjugation.property.pos == property.pos
        && conjugation.property.kind == u16::from(property.kind)
        && conjugation.property.negative == property.negative
        && conjugation.property.formal == property.formal
}

fn support_conjugation(value: &SupportConjugation) -> Result<Conjugation> {
    Ok(Conjugation {
        seq: i64::from(value.seq),
        from: i64::from(value.from),
        via: value.via.map(i64::from),
        property: ConjugationProperty {
            pos: value.property.pos.clone(),
            kind: u8::try_from(value.property.kind)
                .map_err(|_| internal("support conjugation type exceeds u8"))?,
            negative: value.property.negative,
            formal: value.property.formal,
        },
    })
}

fn extend_unique(values: &mut Vec<String>, added: impl IntoIterator<Item = String>) {
    for value in added {
        if !values.contains(&value) {
            values.push(value);
        }
    }
}
