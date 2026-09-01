use super::*;

impl AnalyzerLexicon<'_> {
    pub(super) fn morph(&mut self, value: MorphologyCandidate) -> Result<MaterializedCandidate> {
        let entry_index = self.roots.find_entry_index(value.root_seq)?;
        let surface = utf16(&value.surface);
        let collision =
            self.support
                .collision(value.root_seq, value.route, &surface, &value.rule_ids)?;
        let aliases = self.support.generated_aliases(&value.rule_ids)?;
        let generated = if collision.is_none() {
            self.annotations.generated(value.root_seq, &aliases)?
        } else {
            None
        };
        let generated_via = if collision.is_none() && aliases.len() == 2 {
            self.annotations.generated(value.root_seq, &aliases[..1])?
        } else {
            None
        };
        let physical_seq = collision.as_ref().map_or_else(
            || -i64::from(value.root_seq),
            |row| i64::from(row.collision_seq),
        );
        let root_facts = sequence_facts(self.roots, entry_index)?;
        let self_facts = collision.as_ref().map_or(
            SequenceFacts {
                all_archived: root_facts.all_archived,
                ..SequenceFacts::default()
            },
            |row| SequenceFacts {
                all_archived: row.archived,
                prefer_kana: row.prefer_kana,
                prefer_kana_on_ordinal_zero: row.prefer_kana_on_ordinal_zero,
            },
        );
        let lineage = collision.as_ref().map_or(root_facts, |_| SequenceFacts {
            all_archived: self_facts.all_archived && root_facts.all_archived,
            prefer_kana: self_facts.prefer_kana || root_facts.prefer_kana,
            prefer_kana_on_ordinal_zero: self_facts.prefer_kana_on_ordinal_zero
                || root_facts.prefer_kana_on_ordinal_zero,
        });
        let root_positions = entry_index
            .map(|entry| self.roots.entry_positions(entry))
            .transpose()?
            .unwrap_or_default();
        let source_form =
            self.root_form(&utf16(&value.source_text), value.root_seq, value.route)?;
        let fallback: Vec<_> = value.path.iter().map(inflection_property).collect();
        if fallback.is_empty() || fallback.len() > 2 {
            return Err(internal(
                "morphology materialization requires one or two stages",
            ));
        }
        let root = AnalysisRoot {
            seq: value.root_seq,
            form: utf16(&value.source_form),
            reading: utf16(&value.source_reading),
        };
        let target_n_kanji = collision
            .as_ref()
            .map(|row| row.n_kanji)
            .or_else(|| {
                generated
                    .as_ref()
                    .and_then(|facts| facts.n_kanji.map(u16::from))
            })
            .or(entry_index
                .map(|entry| self.roots.entry_n_kanji(entry).map(u16::from))
                .transpose()?);
        let target_n_kana = collision
            .as_ref()
            .map(|row| row.n_kana)
            .or_else(|| {
                generated
                    .as_ref()
                    .and_then(|facts| facts.n_kana.map(u16::from))
            })
            .or(entry_index
                .map(|entry| self.roots.entry_n_kana(entry).map(u16::from))
                .transpose()?);
        let prefix_collision = if value.rule_ids.len() == 2 {
            value
                .intermediate
                .as_ref()
                .map_or(Ok(None), |intermediate| {
                    self.support.collision(
                        value.root_seq,
                        value.route,
                        &utf16(intermediate),
                        &value.rule_ids[..1],
                    )
                })?
        } else {
            None
        };
        let via_seq = prefix_collision.as_ref().map(|row| row.collision_seq);
        let final_key = StageKey {
            root_seq: value.root_seq,
            aliases: aliases.clone(),
        };
        let prefix_key = (aliases.len() == 2).then(|| StageKey {
            root_seq: value.root_seq,
            aliases: aliases[..1].to_vec(),
        });
        let semantic_members = self.semantic_members(
            &fallback,
            &root,
            entry_index,
            generated.as_ref(),
            generated_via.as_ref(),
            target_n_kanji,
            target_n_kana,
            via_seq,
            &final_key,
            prefix_key.as_ref(),
        )?;
        let inflection = semantic_members
            .first()
            .map_or_else(|| fallback.clone(), |member| member.inflection.clone());
        let conjugations = semantic_members
            .iter()
            .map(|member| {
                let property = member
                    .inflection
                    .last()
                    .ok_or_else(|| internal("semantic member has no final inflection"))?;
                Ok(analysis_conjugation(
                    physical_seq,
                    value.root_seq,
                    property,
                    member.inflection.len() > 1,
                ))
            })
            .collect::<Result<Vec<_>>>()?;
        let definition_seq = collision
            .as_ref()
            .map_or(value.root_seq, |row| row.collision_seq);
        let split = if let Some(row) = &collision {
            (self.score_split)(row.collision_seq, value.route, &surface)?.or((self.score_split)(
                value.root_seq,
                value.route,
                &surface,
            )?)
        } else {
            (self.score_split)(value.root_seq, value.route, &surface)?
        };
        let positions = union_strings([
            root_positions,
            collision
                .as_ref()
                .map_or_else(Vec::new, |row| row.pos.clone()),
            semantic_members
                .iter()
                .flat_map(|member| {
                    member
                        .inflection
                        .iter()
                        .map(|property| property.pos.clone())
                })
                .collect(),
        ]);
        let nokanji = match source_form {
            Some(form) => self.roots.form_nokanji(form)?,
            None => value.route == Route::Kana && value.source_form == value.source_reading,
        };
        let score_facts = ScoreCandidate::Word(WordScoreFacts {
            kind: crate::analyzer_model::ScoreWordKind::Word,
            text: surface.clone(),
            true_text: surface.clone(),
            true_text_follows_text: true,
            route: value.route,
            seq: Some(physical_seq),
            ord: i32::from(value.ord),
            common: None,
            nokanji,
            entry: Some(EntryScoreFacts {
                root: collision.is_some(),
                n_kanji: u32::from(target_n_kanji.unwrap_or(0)),
                primary_nokanji: collision.as_ref().is_some_and(|row| row.primary_nokanji),
            }),
            conjugation_only: true,
            conjugations,
            positions,
            self_facts,
            lineage,
            inherited_common: value.common.map(i32::from),
            inherited_ord: Some(i32::from(value.ord)),
            split,
            suru_break: None,
        });
        let reading = utf16(&value.reading);
        let reading = self
            .annotations
            .hint(definition_seq, value.route, &value.surface, &value.reading)?
            .as_deref()
            .map_or(reading, utf16);
        let physical_group = generated.as_ref().and_then(|facts| facts.physical_group);
        let member_ord = semantic_members
            .first()
            .and_then(|member| member.member_ord);
        Ok(MaterializedCandidate {
            kind: CandidateKind::Simple,
            text: surface.clone(),
            true_text: surface,
            route: value.route,
            reading,
            public_seq: Some(value.root_seq),
            physical_seq: Some(physical_seq),
            physical_key: collision.as_ref().map_or_else(
                || PhysicalKey::Semantic(final_key.clone()),
                |row| PhysicalKey::Sequence(row.collision_seq),
            ),
            physical_group,
            lookup_locators: vec![LookupLocator {
                root_seq: definition_seq,
                aliases: collision.is_none().then_some(aliases),
            }],
            member_ord,
            entry_index,
            root: Some(root),
            inflection,
            score_facts,
            components: Vec::new(),
            counter: None,
            suffix_class: self.support.suffix_class(definition_seq)?,
            definition_seq: Some(definition_seq),
            semantic_members,
            identity_roots: vec![value.root_seq],
            conjugation_selection: ConjugationSelection::Default,
        })
    }

    #[allow(clippy::too_many_arguments)]
    fn semantic_members(
        &self,
        fallback: &[AnalysisInflection],
        root: &AnalysisRoot,
        entry_index: Option<usize>,
        generated: Option<&GeneratedFacts>,
        generated_via: Option<&GeneratedFacts>,
        target_n_kanji: Option<u16>,
        target_n_kana: Option<u16>,
        via_seq: Option<u32>,
        final_key: &StageKey,
        prefix_key: Option<&StageKey>,
    ) -> Result<Vec<SemanticMember>> {
        let first_fallback = fallback
            .first()
            .ok_or_else(|| internal("missing first fallback inflection"))?;
        let mut members = Vec::new();
        let final_members = generated.and_then(|facts| facts.members.as_deref());
        if let Some(final_members) = final_members.filter(|members| !members.is_empty()) {
            for final_member in final_members {
                let final_property = self.exact_property(
                    final_member,
                    fallback
                        .last()
                        .ok_or_else(|| internal("missing fallback inflection"))?,
                )?;
                if fallback.len() == 1 {
                    members.push(member(
                        entry_index,
                        root,
                        vec![final_property],
                        generated.and_then(|facts| facts.physical_group),
                        Some(final_member.member_ord),
                        target_n_kanji,
                        target_n_kana,
                        None,
                        vec![generated.and_then(|facts| facts.physical_group)],
                        vec![Some(final_key.clone())],
                        vec![Some(final_member.member_ord)],
                        vec![Some(final_member.prop_ord)],
                    ));
                    continue;
                }
                let prefixes: Vec<_> = generated_via
                    .and_then(|facts| facts.members.as_deref())
                    .unwrap_or_default()
                    .iter()
                    .filter(|prefix| Some(prefix.member_ord) == final_member.via_member_ord)
                    .collect();
                if prefixes.is_empty() {
                    members.push(member(
                        entry_index,
                        root,
                        vec![first_fallback.clone(), final_property],
                        generated.and_then(|facts| facts.physical_group),
                        Some(final_member.member_ord),
                        target_n_kanji,
                        target_n_kana,
                        via_seq,
                        vec![
                            generated_via.and_then(|facts| facts.physical_group),
                            generated.and_then(|facts| facts.physical_group),
                        ],
                        vec![prefix_key.cloned(), Some(final_key.clone())],
                        vec![final_member.via_member_ord, Some(final_member.member_ord)],
                        vec![None, Some(final_member.prop_ord)],
                    ));
                    continue;
                }
                for prefix in prefixes {
                    members.push(member(
                        entry_index,
                        root,
                        vec![
                            self.exact_property(prefix, first_fallback)?,
                            final_property.clone(),
                        ],
                        generated.and_then(|facts| facts.physical_group),
                        Some(final_member.member_ord),
                        target_n_kanji,
                        target_n_kana,
                        via_seq,
                        vec![
                            generated_via.and_then(|facts| facts.physical_group),
                            generated.and_then(|facts| facts.physical_group),
                        ],
                        vec![prefix_key.cloned(), Some(final_key.clone())],
                        vec![Some(prefix.member_ord), Some(final_member.member_ord)],
                        vec![Some(prefix.prop_ord), Some(final_member.prop_ord)],
                    ));
                }
            }
        } else if fallback.len() == 2
            && generated_via
                .and_then(|facts| facts.members.as_ref())
                .is_some_and(|members| !members.is_empty())
        {
            let via = generated_via
                .ok_or_else(|| internal("generated prefix facts disappeared during lookup"))?;
            let second_fallback = fallback
                .get(1)
                .ok_or_else(|| internal("missing second fallback inflection"))?;
            for prefix in via.members.as_deref().unwrap_or_default() {
                members.push(member(
                    entry_index,
                    root,
                    vec![
                        self.exact_property(prefix, first_fallback)?,
                        second_fallback.clone(),
                    ],
                    generated.and_then(|facts| facts.physical_group),
                    None,
                    target_n_kanji,
                    target_n_kana,
                    via_seq,
                    vec![
                        via.physical_group,
                        generated.and_then(|facts| facts.physical_group),
                    ],
                    vec![prefix_key.cloned(), Some(final_key.clone())],
                    vec![Some(prefix.member_ord), Some(0)],
                    vec![Some(prefix.prop_ord), Some(0)],
                ));
            }
        } else {
            let stages = fallback.len();
            members.push(member(
                entry_index,
                root,
                fallback.to_vec(),
                generated.and_then(|facts| facts.physical_group),
                None,
                target_n_kanji,
                target_n_kana,
                if stages == 2 { via_seq } else { None },
                if stages == 1 {
                    vec![generated.and_then(|facts| facts.physical_group)]
                } else {
                    vec![
                        generated_via.and_then(|facts| facts.physical_group),
                        generated.and_then(|facts| facts.physical_group),
                    ]
                },
                if stages == 1 {
                    vec![Some(final_key.clone())]
                } else {
                    vec![prefix_key.cloned(), Some(final_key.clone())]
                },
                vec![Some(0); stages],
                vec![Some(0); stages],
            ));
        }
        Ok(members)
    }

    fn exact_property(
        &self,
        member: &GeneratedMember,
        fallback: &AnalysisInflection,
    ) -> Result<AnalysisInflection> {
        Ok(AnalysisInflection {
            pos: self
                .morphology
                .position(usize::from(member.property.pos_id))?
                .to_owned(),
            kind: member.property.kind,
            negative: member.property.negative,
            formal: member.property.formal,
            ordinal: fallback.ordinal,
        })
    }

    fn root_form(&mut self, surface: &[u16], seq: u32, route: Route) -> Result<Option<usize>> {
        let key = (surface.to_vec(), seq, route);
        if let Some(cached) = self.root_form_cache.get(&key) {
            return Ok(*cached);
        }
        let found = self.surface.lookup(surface)?;
        let mut result = None;
        if let Some(SurfaceMatch {
            direct: true,
            direct_rank: Some(rank),
            ..
        }) = found
        {
            let first = self.roots.surface_form_start(rank)?;
            let count = self.roots.surface_form_count(rank)?;
            for form in first..first + count {
                let entry = self.roots.form_entry_index(form)?;
                if self.roots.entry_seq(entry)? == seq && self.roots.form_route(form)? == route {
                    result = Some(form);
                    break;
                }
            }
        }
        self.root_form_cache.insert(key, result);
        Ok(result)
    }
}

#[allow(clippy::too_many_arguments)]
fn member(
    entry_index: Option<usize>,
    root: &AnalysisRoot,
    inflection: Vec<AnalysisInflection>,
    physical_group: Option<u32>,
    member_ord: Option<u8>,
    target_n_kanji: Option<u16>,
    target_n_kana: Option<u16>,
    via_seq: Option<u32>,
    stage_groups: Vec<Option<u32>>,
    stage_keys: Vec<Option<StageKey>>,
    stage_member_ords: Vec<Option<u8>>,
    stage_prop_ords: Vec<Option<u16>>,
) -> SemanticMember {
    SemanticMember {
        entry_index,
        root: Some(root.clone()),
        inflection,
        public_seq: Some(root.seq),
        physical_group,
        member_ord,
        target_n_kanji,
        target_n_kana,
        via_seq,
        stage_groups,
        stage_keys,
        stage_member_ords,
        stage_prop_ords,
    }
}
