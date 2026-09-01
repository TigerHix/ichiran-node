use std::collections::{HashMap, hash_map::Entry};

use serde::Serialize;
use serde_json::json;

use super::conjugations::conjugation_forest;
use super::descriptions;
use super::senses::senses;
use super::{
    LegacyChunk, LegacyCounter, LegacyDetailedOutput, LegacyGloss, LegacyOptions, LegacyOrdinal,
    LegacyPath, LegacySequence, LegacyToken, LegacyWordFacts, token_romanized,
};
use crate::annotations::AnalyzerAnnotations;
use crate::details::{DetailEntry, DetailRange, DetailStore};
use crate::dto::{
    AnalysisAlternative, AnalysisChunk, AnalysisComponent, AnalysisResult, AnalysisRoot,
    AnalysisToken, LegacyConjugationSelection, LegacyPresentationFacts, LegacySemanticMember,
    PublicRoute, Utf16Text,
};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::{MorphologyProperty, Route};
use crate::roots::RootPayload;
use crate::support::AnalyzerSupport;
use crate::surface::SurfaceIndex;

pub(crate) struct LegacyContext<'a> {
    pub roots: &'a RootPayload,
    pub support: &'a AnalyzerSupport,
    pub surface: &'a SurfaceIndex,
    pub annotations: &'a mut AnalyzerAnnotations,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LegacyDetailRequest {
    pub entry_index: u32,
    pub range: DetailRange,
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum LegacyDetailedResult {
    Ready(LegacyDetailedOutput),
    MissingDetail(LegacyDetailRequest),
}

#[derive(Default)]
pub(crate) struct LegacyDetailedSession {
    entries: HashMap<usize, DetailEntry>,
}

pub(super) enum AttemptError {
    Kernel(KernelError),
    Missing(LegacyDetailRequest),
}

pub(super) type Attempt<T> = std::result::Result<T, AttemptError>;

impl From<KernelError> for AttemptError {
    fn from(value: KernelError) -> Self {
        Self::Kernel(value)
    }
}

impl LegacyDetailedSession {
    pub fn serialize(
        &mut self,
        result: &AnalysisResult,
        details: &DetailStore,
        context: &mut LegacyContext<'_>,
        options: &LegacyOptions<'_>,
    ) -> Result<LegacyDetailedResult> {
        match self.try_serialize(result, details, context, options) {
            Ok(value) => Ok(LegacyDetailedResult::Ready(value)),
            Err(AttemptError::Missing(request)) => Ok(LegacyDetailedResult::MissingDetail(request)),
            Err(AttemptError::Kernel(error)) => Err(error),
        }
    }

    fn try_serialize(
        &mut self,
        result: &AnalysisResult,
        details: &DetailStore,
        context: &mut LegacyContext<'_>,
        options: &LegacyOptions<'_>,
    ) -> Attempt<LegacyDetailedOutput> {
        let mut output = Vec::new();
        for chunk in &result.chunks {
            match chunk {
                AnalysisChunk::Misc { text, .. } => output.push(LegacyChunk::Misc(text.clone())),
                AnalysisChunk::Word { paths, .. } => {
                    let mut rendered_paths = Vec::new();
                    for path in paths {
                        let mut words = Vec::new();
                        for token in &path.tokens {
                            let romanized = token_romanized(token, options.method);
                            let property = options.word_property.map_or_else(
                                || json!([]),
                                |property| property(romanized.units(), token),
                            );
                            words.push(LegacyToken(
                                romanized,
                                self.detailed_token(token, details, context)?,
                                property,
                            ));
                        }
                        rendered_paths.push(LegacyPath(words, path.score));
                    }
                    output.push(LegacyChunk::Paths(rendered_paths));
                }
            }
        }
        Ok(output)
    }

    pub(super) fn entry<'a>(
        &'a mut self,
        entry_index: Option<usize>,
        details: &DetailStore,
    ) -> Attempt<Option<&'a DetailEntry>> {
        let Some(index) = entry_index else {
            return Ok(None);
        };
        match self.entries.entry(index) {
            Entry::Occupied(entry) => Ok(Some(entry.into_mut())),
            Entry::Vacant(slot) => {
                let wire_index = u32::try_from(index).map_err(|_| {
                    KernelError::new(
                        ErrorCode::OutOfRange,
                        "legacy detail entry exceeds the detail format limit",
                    )
                })?;
                let Some(entry) = details.entry_cached(wire_index)? else {
                    return Err(AttemptError::Missing(LegacyDetailRequest {
                        entry_index: wire_index,
                        range: details.range(wire_index)?,
                    }));
                };
                Ok(Some(slot.insert(entry)))
            }
        }
    }

    fn detailed_token(
        &mut self,
        token: &AnalysisToken,
        details: &DetailStore,
        context: &mut LegacyContext<'_>,
    ) -> Attempt<LegacyGloss> {
        if token.alternatives.len() > 1 {
            let mut alternatives = Vec::new();
            for alternative in &token.alternatives {
                alternatives.push(self.detailed_alternative(alternative, details, context)?);
            }
            return Ok(LegacyGloss::alternative(alternatives));
        }
        let route = if token.entity && token.root.is_none() {
            Route::Kana
        } else {
            match token.route {
                PublicRoute::Kana => Route::Kana,
                PublicRoute::Kanji => Route::Kanji,
                PublicRoute::Gap => Route::Kana,
            }
        };
        self.detailed_word(
            DetailWord {
                text: &token.text,
                route,
                reading: &token.reading,
                score: token.score,
                entry_index: token.entry_index,
                root: token.root.as_ref(),
                inflection: &token.inflection,
                components: &token.components,
                counter: token.counter.as_ref(),
                entity: token.entity,
                suffix: false,
                component: false,
                facts: token.legacy.as_ref(),
            },
            details,
            context,
        )
    }

    fn detailed_alternative(
        &mut self,
        alternative: &AnalysisAlternative,
        details: &DetailStore,
        context: &mut LegacyContext<'_>,
    ) -> Attempt<LegacyGloss> {
        self.detailed_word(
            DetailWord {
                text: &alternative.text,
                route: alternative.route,
                reading: &alternative.reading,
                score: alternative.score,
                entry_index: alternative.entry_index,
                root: alternative.root.as_ref(),
                inflection: &alternative.inflection,
                components: &alternative.components,
                counter: alternative.counter.as_ref(),
                entity: false,
                suffix: false,
                component: false,
                facts: alternative.legacy.as_ref(),
            },
            details,
            context,
        )
    }

    fn detailed_component(
        &mut self,
        component: &AnalysisComponent,
        details: &DetailStore,
        context: &mut LegacyContext<'_>,
    ) -> Attempt<LegacyGloss> {
        self.detailed_word(
            DetailWord {
                text: &component.text,
                route: component.route,
                reading: &component.reading,
                score: 0.0,
                entry_index: component.entry_index,
                root: component.root.as_ref(),
                inflection: &component.inflection,
                components: &[],
                counter: None,
                entity: false,
                suffix: !component.primary,
                component: true,
                facts: component.legacy.as_ref(),
            },
            details,
            context,
        )
    }

    fn detailed_word(
        &mut self,
        word: DetailWord<'_>,
        details: &DetailStore,
        context: &mut LegacyContext<'_>,
    ) -> Attempt<LegacyGloss> {
        let root_seq = word.root.map(|root| root.seq);
        let root_form = word.root.map_or_else(
            || word.text.clone(),
            |root| Utf16Text::from(root.form.as_str()),
        );
        let root_reading = word.root.map_or_else(
            || word.reading.clone(),
            |root| Utf16Text::from(root.reading.as_str()),
        );
        let mut output = LegacyGloss {
            reading: Some(reading_label(
                word.route,
                word.text,
                word.reading,
                root_seq.and(word.counter),
            )),
            text: Some(word.text.clone()),
            kana: Some(word.reading.clone()),
            score: Some(word.score),
            compound: None,
            components: None,
            counter: None,
            seq: None,
            gloss: None,
            suffix: None,
            conj: None,
            alternative: None,
            info: None,
        };
        let selection = word.facts.map_or(
            if word.component {
                LegacyConjugationSelection::Explicit
            } else {
                LegacyConjugationSelection::Default
            },
            |facts| facts.conjugation_selection,
        );
        let definition_seq = word.facts.map_or(root_seq, |facts| facts.definition_seq);
        output.info = Some(LegacyWordFacts {
            definition_seq,
            conjugation_selection: selection,
            inflected: !word.inflection.is_empty(),
        });
        if !word.components.is_empty() {
            output.compound = Some(
                word.components
                    .iter()
                    .map(|component| component.text.clone())
                    .collect(),
            );
            let mut components = Vec::new();
            for component in word.components {
                components.push(self.detailed_component(component, details, context)?);
            }
            output.components = Some(components);
            return Ok(output);
        }
        let fallback_root = match root_seq {
            Some(seq) => Some(AnalysisRoot {
                seq,
                form: root_form_string(&root_form)?,
                reading: root_form_string(&root_reading)?,
            }),
            None => None,
        };
        let fallback_member = LegacySemanticMember {
            entry_index: word.entry_index,
            root: fallback_root,
            inflection: word.inflection.to_vec(),
            stage_groups: vec![None; word.inflection.len()],
            stage_keys: vec![None; word.inflection.len()],
            stage_member_ords: vec![None; word.inflection.len()],
            stage_prop_ords: Vec::new(),
            member_ord: None,
        };
        let members = word.facts.map_or_else(
            || std::slice::from_ref(&fallback_member),
            |facts| facts.semantic_members.as_slice(),
        );
        let entry = self.entry(word.entry_index, details)?.cloned();
        if let Some(seq) = root_seq {
            let has_direct = members.iter().any(|member| member.inflection.is_empty());
            let mut semantic_seqs = if let Some(facts) = word.facts {
                facts.identity_roots.clone()
            } else {
                members
                    .iter()
                    .filter_map(|member| member.root.as_ref().map(|root| root.seq))
                    .collect()
            };
            semantic_seqs.sort_unstable();
            semantic_seqs.dedup();
            output.seq = Some(if has_direct || semantic_seqs.is_empty() {
                LegacySequence::One(seq)
            } else if semantic_seqs.len() == 1 {
                LegacySequence::One(semantic_seqs[0])
            } else {
                LegacySequence::Many(semantic_seqs)
            });
        }
        let sense_route = if word.inflection.is_empty() {
            word.route
        } else if crate::characters::test_word(
            root_form.units(),
            crate::characters::CharClass::Kana,
        ) {
            Route::Kana
        } else {
            Route::Kanji
        };
        let counter_filter = word.counter.map(|_| vec!["ctr".to_owned()]);
        let mut root_gloss = match (&entry, word.entry_index) {
            (Some(entry), Some(index)) => senses(
                entry,
                index,
                context,
                sense_route,
                &root_form_string(&root_form)?,
                &root_form_string(&root_reading)?,
                counter_filter.as_deref(),
            )?,
            _ => Vec::new(),
        };
        let proper_noun = super::LegacySense {
            pos: "[n-pr]".to_owned(),
            gloss: "proper noun (named entity)".to_owned(),
            field: None,
            info: None,
        };
        if word.entity && !root_gloss.iter().any(|sense| sense.pos == "[n-pr]") {
            root_gloss.insert(0, proper_noun.clone());
        }
        if let Some((value, ordinal)) = word.counter {
            if !root_gloss.is_empty() {
                output.gloss = Some(root_gloss);
            }
            output.counter = Some(LegacyCounter {
                value: value.clone(),
                ordinal: if *ordinal {
                    LegacyOrdinal::Yes(true)
                } else {
                    LegacyOrdinal::No(Vec::new())
                },
            });
            return Ok(output);
        }
        let mut suffix_class = word
            .facts
            .and_then(|facts| facts.suffix_class.as_deref())
            .map(str::to_owned);
        if suffix_class.is_none()
            && let Some(seq) = word.facts.and_then(|facts| facts.definition_seq)
        {
            suffix_class = context.support.suffix_class(seq)?;
        }
        if suffix_class.is_none()
            && let Some(seq) = root_seq
        {
            suffix_class = context.support.suffix_class(seq)?;
        }
        let suffix = word
            .suffix
            .then(|| {
                suffix_class
                    .as_deref()
                    .and_then(descriptions::suffix)
                    .or_else(|| root_seq.and_then(|seq| descriptions::suffix(&seq.to_string())))
            })
            .flatten();
        output.suffix = suffix.map(str::to_owned);
        if output.suffix.is_none()
            && word.inflection.is_empty()
            && word.facts.is_none_or(|facts| {
                facts.conjugation_selection != LegacyConjugationSelection::Explicit
            })
            && !root_gloss.is_empty()
        {
            output.gloss = Some(root_gloss);
        }
        if root_seq.is_none() {
            return Ok(output);
        }
        output.conj = Some(conjugation_forest(
            members, self, details, context, selection, word.route,
        )?);
        if word.entity
            && output
                .gloss
                .as_ref()
                .is_none_or(|gloss| !gloss.iter().any(|sense| sense.pos == "[n-pr]"))
        {
            output
                .gloss
                .get_or_insert_with(Vec::new)
                .insert(0, proper_noun);
        }
        Ok(output)
    }
}

struct DetailWord<'a> {
    text: &'a Utf16Text,
    route: Route,
    reading: &'a Utf16Text,
    score: f64,
    entry_index: Option<usize>,
    root: Option<&'a AnalysisRoot>,
    inflection: &'a [MorphologyProperty],
    components: &'a [AnalysisComponent],
    counter: Option<&'a (String, bool)>,
    entity: bool,
    suffix: bool,
    component: bool,
    facts: Option<&'a LegacyPresentationFacts>,
}

fn reading_label(
    route: Route,
    text: &Utf16Text,
    reading: &Utf16Text,
    counter: Option<&(String, bool)>,
) -> Utf16Text {
    if route != Route::Kanji && counter.is_none() {
        return text.clone();
    }
    let mut value = text.units().to_vec();
    value.extend(" 【".encode_utf16());
    value.extend_from_slice(reading.units());
    value.push('】' as u16);
    Utf16Text::from_units(&value)
}

fn root_form_string(value: &Utf16Text) -> Result<String> {
    String::from_utf16(value.units()).map_err(|_| {
        KernelError::new(
            ErrorCode::Internal,
            "legacy dictionary root contains malformed UTF-16",
        )
    })
}
