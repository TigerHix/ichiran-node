use super::*;

#[derive(Eq, PartialEq)]
enum GroupKey {
    Physical(u32),
    Key(PhysicalKey),
}

#[derive(Hash, Eq, PartialEq)]
struct SemanticIdentity {
    public_seq: Option<u32>,
    inflection: Vec<AnalysisInflection>,
    stage_groups: Vec<Option<u32>>,
    stage_keys: Vec<Option<StageKey>>,
    stage_member_ords: Vec<Option<u8>>,
    stage_prop_ords: Vec<Option<u16>>,
}

impl From<&SemanticMember> for SemanticIdentity {
    fn from(member: &SemanticMember) -> Self {
        Self {
            public_seq: member.public_seq,
            inflection: member.inflection.clone(),
            stage_groups: member.stage_groups.clone(),
            stage_keys: member.stage_keys.clone(),
            stage_member_ords: member.stage_member_ords.clone(),
            stage_prop_ords: member.stage_prop_ords.clone(),
        }
    }
}

impl AnalyzerLexicon<'_> {
    pub(super) fn group_physical(
        &self,
        values: Vec<MaterializedCandidate>,
    ) -> Result<Vec<MaterializedCandidate>> {
        let mut groups: Vec<(GroupKey, Vec<MaterializedCandidate>)> = Vec::new();
        for value in values {
            let key = value.physical_group.map_or_else(
                || GroupKey::Key(value.physical_key.clone()),
                GroupKey::Physical,
            );
            if let Some((_, members)) = groups.iter_mut().find(|(existing, _)| *existing == key) {
                members.push(value);
            } else {
                groups.push((key, vec![value]));
            }
        }
        groups
            .into_iter()
            .map(|(_, mut members)| {
                if members.len() == 1 {
                    members
                        .pop()
                        .ok_or_else(|| internal("physical group disappeared"))
                } else {
                    self.merge_physical(members)
                }
            })
            .collect()
    }

    fn merge_physical(
        &self,
        mut values: Vec<MaterializedCandidate>,
    ) -> Result<MaterializedCandidate> {
        let lookup_locators = lookup_locators(&values);
        values.sort_by_key(|value| {
            (
                usize::from(!value.inflection.is_empty()),
                value.member_ord.map_or(usize::MAX, usize::from),
            )
        });
        let mut base = values
            .first()
            .cloned()
            .ok_or_else(|| internal("cannot merge an empty physical group"))?;
        let word_values: Vec<_> = values
            .iter()
            .filter_map(|value| match &value.score_facts {
                ScoreCandidate::Word(facts) => Some((value, facts)),
                ScoreCandidate::Compound(_) => None,
            })
            .collect();
        if word_values.len() != values.len() {
            return Ok(base);
        }
        let all_conjugations: Vec<_> = word_values
            .iter()
            .flat_map(|(_, facts)| facts.conjugations.iter().cloned())
            .collect();
        let secondary_only = !all_conjugations.is_empty()
            && all_conjugations
                .iter()
                .all(|conjugation| conjugation.via.is_some());
        let selected: Vec<_> = word_values
            .iter()
            .copied()
            .filter(|(_, facts)| {
                facts.conjugations.iter().any(|conjugation| {
                    if secondary_only {
                        conjugation.via.is_some()
                    } else {
                        conjugation.via.is_none()
                    }
                })
            })
            .collect();
        let target = word_values
            .iter()
            .copied()
            .find(|(value, _)| value.inflection.is_empty());
        let mut scoring = Vec::with_capacity(selected.len() + usize::from(target.is_some()));
        if let Some((_, facts)) = target {
            scoring.push(facts);
        }
        scoring.extend(selected.iter().map(|(_, facts)| *facts));
        let inherited_common = selected
            .iter()
            .filter_map(|(_, facts)| facts.inherited_common)
            .min_by(|left, right| match (*left == 0, *right == 0) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => left.cmp(right),
            });
        let inherited_ord = selected
            .iter()
            .filter_map(|(_, facts)| facts.inherited_ord)
            .min();
        let base_facts = word_values
            .first()
            .map(|(_, facts)| *facts)
            .ok_or_else(|| internal("physical word group has no score facts"))?;
        let entries: Vec<_> = word_values
            .iter()
            .filter_map(|(_, facts)| facts.entry)
            .collect();
        let all_members: Vec<_> = values
            .iter()
            .flat_map(|value| value.semantic_members.iter().cloned())
            .collect();
        let selected_members: Vec<_> = all_members
            .iter()
            .filter(|member| {
                !member.inflection.is_empty()
                    && if secondary_only {
                        member.inflection.len() > 1
                    } else {
                        member.inflection.len() == 1
                    }
            })
            .cloned()
            .collect();
        let selected_sequence = self.member_sequence_facts(&all_members, &selected_members)?;
        let raw_root_facts = all_members
            .iter()
            .filter(|member| !member.inflection.is_empty())
            .map(|member| sequence_facts(self.roots, member.entry_index))
            .collect::<Result<Vec<_>>>()?;
        let target_facts = target.map_or(base_facts.self_facts, |(_, facts)| facts.self_facts);
        let self_facts = SequenceFacts {
            all_archived: target_facts.all_archived
                || raw_root_facts.iter().any(|facts| facts.all_archived),
            prefer_kana: target_facts.prefer_kana,
            prefer_kana_on_ordinal_zero: target_facts.prefer_kana_on_ordinal_zero,
        };
        let lineage = SequenceFacts {
            all_archived: self_facts.all_archived
                && !selected_sequence.is_empty()
                && selected_sequence.iter().all(|facts| facts.all_archived),
            prefer_kana: self_facts.prefer_kana
                || selected_sequence.iter().any(|facts| facts.prefer_kana),
            prefer_kana_on_ordinal_zero: self_facts.prefer_kana_on_ordinal_zero
                || selected_sequence
                    .iter()
                    .any(|facts| facts.prefer_kana_on_ordinal_zero),
        };
        let positions = union_strings(scoring.iter().map(|facts| facts.positions.clone()));
        let score_facts = ScoreCandidate::Word(WordScoreFacts {
            seq: base.physical_seq,
            ord: word_values
                .iter()
                .map(|(_, facts)| facts.ord)
                .min()
                .unwrap_or(base_facts.ord),
            common: target.and_then(|(_, facts)| facts.common),
            nokanji: target.map_or(base_facts.nokanji, |(_, facts)| facts.nokanji),
            entry: entries.first().map(|first| EntryScoreFacts {
                root: entries.iter().any(|entry| entry.root),
                n_kanji: first.n_kanji,
                primary_nokanji: entries.iter().any(|entry| entry.primary_nokanji),
            }),
            conjugation_only: word_values.iter().all(|(_, facts)| facts.conjugation_only),
            conjugations: all_conjugations,
            positions,
            self_facts,
            lineage,
            inherited_common,
            inherited_ord,
            split: word_values
                .iter()
                .find_map(|(_, facts)| facts.split.clone()),
            ..base_facts.clone()
        });
        let mut semantic_members = all_members;
        semantic_members.sort_by_key(|member| member.member_ord.map_or(usize::MAX, usize::from));
        let mut seen = HashSet::new();
        semantic_members.retain(|member| seen.insert(SemanticIdentity::from(member)));
        let mut identity_roots = Vec::new();
        for seq in semantic_members
            .iter()
            .filter_map(|member| member.root.as_ref().map(|root| root.seq))
        {
            if !identity_roots.contains(&seq) {
                identity_roots.push(seq);
            }
        }
        base.physical_group = values.iter().find_map(|value| value.physical_group);
        base.lookup_locators = lookup_locators;
        base.member_ord = values.iter().filter_map(|value| value.member_ord).min();
        base.score_facts = score_facts;
        base.semantic_members = semantic_members;
        base.identity_roots = identity_roots;
        Ok(base)
    }

    fn member_sequence_facts(
        &self,
        all_members: &[SemanticMember],
        selected_members: &[SemanticMember],
    ) -> Result<Vec<SequenceFacts>> {
        let mut archived_intermediates = HashSet::new();
        for member in all_members {
            if let (Some(via_seq), Some(entry)) = (member.via_seq, member.entry_index)
                && self.roots.entry_archived(entry)?
            {
                archived_intermediates.insert(via_seq);
            }
        }
        selected_members
            .iter()
            .map(|member| {
                let mut facts = sequence_facts(self.roots, member.entry_index)?;
                let root_seq = member
                    .root
                    .as_ref()
                    .map(|root| root.seq)
                    .or(member.public_seq);
                if root_seq.is_some_and(|seq| archived_intermediates.contains(&seq)) {
                    facts.all_archived = true;
                }
                Ok(facts)
            })
            .collect()
    }
}

fn lookup_locators(values: &[MaterializedCandidate]) -> Vec<LookupLocator> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for locator in values.iter().flat_map(|value| value.lookup_locators.iter()) {
        if seen.insert(locator.clone()) {
            result.push(locator.clone());
        }
    }
    result
}
