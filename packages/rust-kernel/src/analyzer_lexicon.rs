use std::collections::{HashMap, HashSet};

use crate::analyzer_model::{
    Conjugation, EntryScoreFacts, ScoreCandidate, ScoreSplit, SequenceFacts, WordScoreFacts,
};
use crate::annotations::{AnalyzerAnnotations, GeneratedFacts, GeneratedMember};
use crate::characters::as_hiragana;
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::{Morphology, MorphologyCandidate, MorphologyProperty, Route};
use crate::roots::RootPayload;
use crate::support::AnalyzerSupport;
use crate::surface::{SurfaceIndex, SurfaceMatch};
use crate::text::{string as utf16_string, utf16};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct AnalysisInflection {
    pub pos: String,
    pub kind: u8,
    pub negative: Option<bool>,
    pub formal: Option<bool>,
    pub ordinal: u8,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct AnalysisRoot {
    pub seq: u32,
    pub form: Vec<u16>,
    pub reading: Vec<u16>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct SemanticMember {
    pub entry_index: Option<usize>,
    pub root: Option<AnalysisRoot>,
    pub inflection: Vec<AnalysisInflection>,
    pub public_seq: Option<u32>,
    pub physical_group: Option<u32>,
    pub member_ord: Option<u8>,
    pub target_n_kanji: Option<u16>,
    pub target_n_kana: Option<u16>,
    pub via_seq: Option<u32>,
    pub stage_groups: Vec<Option<u32>>,
    pub stage_keys: Vec<Option<StageKey>>,
    pub stage_member_ords: Vec<Option<u8>>,
    pub stage_prop_ords: Vec<Option<u16>>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct StageKey {
    pub root_seq: u32,
    pub aliases: Vec<u16>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PhysicalKey {
    Sequence(u32),
    Semantic(StageKey),
    Counter(Vec<u16>),
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LookupLocator {
    pub root_seq: u32,
    pub aliases: Option<Vec<u16>>,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum CandidateKind {
    Simple,
    Proxy,
    Compound,
    Counter,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CandidateComponent {
    pub text: Vec<u16>,
    pub true_text: Option<Vec<u16>>,
    pub route: Route,
    pub reading: Vec<u16>,
    pub entry_index: Option<usize>,
    pub root: Option<AnalysisRoot>,
    pub inflection: Vec<AnalysisInflection>,
    pub primary: bool,
    pub public_seq: Option<u32>,
    pub physical_key: PhysicalKey,
    pub physical_group: Option<u32>,
    pub suffix_class: Option<String>,
    pub definition_seq: Option<u32>,
    pub semantic_members: Vec<SemanticMember>,
    pub identity_roots: Vec<u32>,
    pub conjugation_selection: ConjugationSelection,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MaterializedCandidate {
    pub kind: CandidateKind,
    pub text: Vec<u16>,
    pub true_text: Vec<u16>,
    pub route: Route,
    pub reading: Vec<u16>,
    pub public_seq: Option<u32>,
    pub physical_seq: Option<i64>,
    pub physical_key: PhysicalKey,
    pub physical_group: Option<u32>,
    pub lookup_locators: Vec<LookupLocator>,
    pub member_ord: Option<u8>,
    pub entry_index: Option<usize>,
    pub root: Option<AnalysisRoot>,
    pub inflection: Vec<AnalysisInflection>,
    pub score_facts: ScoreCandidate,
    pub components: Vec<CandidateComponent>,
    pub counter: Option<(String, bool)>,
    pub suffix_class: Option<String>,
    pub definition_seq: Option<u32>,
    pub semantic_members: Vec<SemanticMember>,
    pub identity_roots: Vec<u32>,
    pub conjugation_selection: ConjugationSelection,
}

impl MaterializedCandidate {
    pub fn as_component(&self, primary: bool) -> CandidateComponent {
        CandidateComponent {
            text: self.text.clone(),
            true_text: (self.true_text != self.text).then(|| self.true_text.clone()),
            route: self.route,
            reading: self.reading.clone(),
            entry_index: self.entry_index,
            root: self.root.clone(),
            inflection: self.inflection.clone(),
            primary,
            public_seq: self.public_seq,
            physical_key: self.physical_key.clone(),
            physical_group: self.physical_group,
            suffix_class: self.suffix_class.clone(),
            definition_seq: self.definition_seq,
            semantic_members: self.semantic_members.clone(),
            identity_roots: self.identity_roots.clone(),
            conjugation_selection: self.conjugation_selection,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ConjugationSelection {
    Default,
    Explicit,
    Root,
}

fn analysis_conjugation(
    physical_seq: i64,
    root_seq: u32,
    property: &AnalysisInflection,
    secondary: bool,
) -> Conjugation {
    Conjugation {
        seq: physical_seq,
        from: i64::from(root_seq),
        via: secondary.then_some(-i64::from(root_seq)),
        property: crate::analyzer_model::ConjugationProperty {
            pos: property.pos.clone(),
            kind: property.kind,
            negative: property.negative,
            formal: property.formal,
        },
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct SplitKey {
    definition_seq: u32,
    route: Route,
    surface: Vec<u16>,
}

pub struct AnalyzerLexicon<'a> {
    surface: &'a SurfaceIndex,
    roots: &'a RootPayload,
    morphology: &'a Morphology,
    support: &'a AnalyzerSupport,
    annotations: &'a mut AnalyzerAnnotations,
    lexical_cache: HashMap<Vec<u16>, Vec<MaterializedCandidate>>,
    full_cache: HashMap<Vec<u16>, Vec<MaterializedCandidate>>,
    score_split_cache: HashMap<SplitKey, Option<ScoreSplit>>,
    score_split_in_progress: HashSet<SplitKey>,
    root_form_cache: HashMap<(Vec<u16>, u32, Route), Option<usize>>,
}

impl<'a> AnalyzerLexicon<'a> {
    pub fn new(
        surface: &'a SurfaceIndex,
        roots: &'a RootPayload,
        morphology: &'a Morphology,
        support: &'a AnalyzerSupport,
        annotations: &'a mut AnalyzerAnnotations,
    ) -> Self {
        Self {
            surface,
            roots,
            morphology,
            support,
            annotations,
            lexical_cache: HashMap::new(),
            full_cache: HashMap::new(),
            score_split_cache: HashMap::new(),
            score_split_in_progress: HashSet::new(),
            root_form_cache: HashMap::new(),
        }
    }

    pub fn reset(&mut self) {
        self.lexical_cache.clear();
        self.full_cache.clear();
        self.score_split_cache.clear();
        self.score_split_in_progress.clear();
        self.root_form_cache.clear();
    }

    pub fn lexical(&mut self, surface: &[u16]) -> Result<Vec<MaterializedCandidate>> {
        if let Some(cached) = self.lexical_cache.get(surface) {
            return Ok(cached.clone());
        }
        let Some(found) = self.surface.lookup(surface)? else {
            return Ok(Vec::new());
        };
        let mut values = Vec::new();
        if found.direct
            && let Some(rank) = found.direct_rank
        {
            let first = self.roots.surface_form_start(rank)?;
            let count = self.roots.surface_form_count(rank)?;
            for form in first..first + count {
                values.push(self.direct(surface, form)?);
            }
        }
        let morphology = if found.morphology {
            self.morphology.lookup(surface, found.route)?
        } else {
            Vec::new()
        };
        let has_morphology = !morphology.is_empty();
        for candidate in morphology {
            values.push(self.morph(candidate)?);
        }
        let mut values = self.group_physical(dedupe(values)?)?;
        if has_morphology && values.len() > 1 {
            self.sort_lookup_order(found.route, surface, &mut values)?;
        }
        self.lexical_cache.insert(surface.to_vec(), values.clone());
        Ok(values)
    }

    /// Stable de-duplication using the frozen TypeScript candidate-key fields.
    pub(crate) fn dedupe_candidates(
        &self,
        values: Vec<MaterializedCandidate>,
    ) -> Result<Vec<MaterializedCandidate>> {
        dedupe(values)
    }

    /// Materialize hiragana lexical rows as katakana proxies.
    pub(crate) fn katakana_proxy(
        &mut self,
        surface: &[u16],
        existing_simple: &[MaterializedCandidate],
    ) -> Result<Vec<MaterializedCandidate>> {
        let hiragana = as_hiragana(surface);
        if hiragana == surface {
            return Ok(Vec::new());
        }
        let excluded = existing_simple
            .iter()
            .map(|value| value.public_seq)
            .collect::<HashSet<_>>();
        let mut result = Vec::new();
        for mut candidate in self.lexical(&hiragana)? {
            if candidate.kind != CandidateKind::Simple
                || !candidate.inflection.is_empty()
                || excluded.contains(&candidate.public_seq)
            {
                continue;
            }
            let ScoreCandidate::Word(mut facts) = candidate.score_facts else {
                continue;
            };
            facts.text = surface.to_vec();
            facts.true_text_follows_text = false;
            candidate.kind = CandidateKind::Proxy;
            candidate.text = surface.to_vec();
            candidate.reading = surface.to_vec();
            candidate.score_facts = ScoreCandidate::Word(facts);
            candidate.components.clear();
            result.push(candidate);
        }
        Ok(result)
    }

    /// Render and materialize counter variants through this request-local lexicon.
    pub(crate) fn counter_candidates(
        &mut self,
        number_text: &[u16],
        _counter_text: &[u16],
        unique: bool,
        variants: &[crate::support::SupportCounterVariant],
    ) -> Result<Vec<MaterializedCandidate>> {
        let roots = self.roots;
        let mut result = Vec::new();
        for variant in variants {
            let Some(rendered) =
                crate::analyzer_counters::materialize_counter(number_text, variant, unique)?
            else {
                continue;
            };
            result.push(crate::analyzer_counters::materialize_counter_candidate(
                roots, self, rendered, variant,
            )?);
        }
        dedupe(result)
    }

    fn direct(&mut self, surface: &[u16], form: usize) -> Result<MaterializedCandidate> {
        let entry_index = self.roots.form_entry_index(form)?;
        let seq = self.roots.entry_seq(entry_index)?;
        let route = self.roots.form_route(form)?;
        let best = self
            .roots
            .resolve_surface_reference(self.roots.form_best_reference(form)?, |rank| {
                self.surface.direct_surface(rank)
            })?;
        let best = best.as_deref().map(utf16);
        let root = AnalysisRoot {
            seq,
            form: if route == Route::Kanji {
                surface.to_vec()
            } else {
                best.clone().unwrap_or_else(|| surface.to_vec())
            },
            reading: if route == Route::Kanji {
                best.clone().unwrap_or_else(|| surface.to_vec())
            } else {
                surface.to_vec()
            },
        };
        let facts = sequence_facts(self.roots, Some(entry_index))?;
        let split = self.score_split(seq, route, surface)?;
        let score_facts = ScoreCandidate::Word(WordScoreFacts {
            kind: crate::analyzer_model::ScoreWordKind::Word,
            text: surface.to_vec(),
            true_text: surface.to_vec(),
            true_text_follows_text: true,
            route,
            seq: Some(i64::from(seq)),
            ord: i32::from(self.roots.form_ordinal(form)?),
            common: self.roots.form_common(form)?.map(i32::from),
            nokanji: self.roots.form_nokanji(form)?,
            entry: Some(EntryScoreFacts {
                root: true,
                n_kanji: u32::from(self.roots.entry_n_kanji(entry_index)?),
                primary_nokanji: self.roots.entry_primary_nokanji(entry_index)?,
            }),
            conjugation_only: false,
            conjugations: Vec::new(),
            positions: self.roots.entry_positions(entry_index)?,
            self_facts: facts,
            lineage: facts,
            inherited_common: None,
            inherited_ord: None,
            split,
            suru_break: None,
        });
        let surface_text = utf16_string(surface, "direct lexical surface")?;
        let reading_text = utf16_string(&root.reading, "direct lexical reading")?;
        let reading = self
            .annotations
            .hint(seq, route, &surface_text, &reading_text)?
            .as_deref()
            .map_or_else(|| root.reading.clone(), utf16);
        let member = SemanticMember {
            entry_index: Some(entry_index),
            root: Some(root.clone()),
            inflection: Vec::new(),
            public_seq: Some(seq),
            physical_group: None,
            member_ord: None,
            target_n_kanji: Some(u16::from(self.roots.entry_n_kanji(entry_index)?)),
            target_n_kana: Some(u16::from(self.roots.entry_n_kana(entry_index)?)),
            via_seq: None,
            stage_groups: Vec::new(),
            stage_keys: Vec::new(),
            stage_member_ords: Vec::new(),
            stage_prop_ords: Vec::new(),
        };
        Ok(MaterializedCandidate {
            kind: CandidateKind::Simple,
            text: surface.to_vec(),
            true_text: surface.to_vec(),
            route,
            reading,
            public_seq: Some(seq),
            physical_seq: Some(i64::from(seq)),
            physical_key: PhysicalKey::Sequence(seq),
            physical_group: None,
            lookup_locators: vec![LookupLocator {
                root_seq: seq,
                aliases: None,
            }],
            member_ord: None,
            entry_index: Some(entry_index),
            root: Some(root),
            inflection: Vec::new(),
            score_facts,
            components: Vec::new(),
            counter: None,
            suffix_class: self.support.suffix_class(seq)?,
            definition_seq: Some(seq),
            semantic_members: vec![member],
            identity_roots: vec![seq],
            conjugation_selection: ConjugationSelection::Default,
        })
    }

    fn sort_lookup_order(
        &mut self,
        route: Route,
        surface: &[u16],
        values: &mut [MaterializedCandidate],
    ) -> Result<()> {
        let surface = utf16_string(surface, "lexical lookup surface")?;
        let mut ranked = Vec::with_capacity(values.len());
        for (index, value) in values.iter().enumerate() {
            if value.lookup_locators.is_empty() {
                return Err(internal(format!(
                    "Incomplete analyzer lookup order for {surface:?}"
                )));
            }
            let mut ranks = HashSet::new();
            for locator in &value.lookup_locators {
                let rank = self
                    .annotations
                    .lookup_order(
                        route,
                        &surface,
                        locator.root_seq,
                        locator.aliases.as_deref(),
                    )?
                    .ok_or_else(|| {
                        let aliases = locator.aliases.as_deref().map_or_else(
                            || "direct".to_owned(),
                            |values| {
                                values
                                    .iter()
                                    .map(u16::to_string)
                                    .collect::<Vec<_>>()
                                    .join(",")
                            },
                        );
                        internal(format!(
                            "Incomplete analyzer lookup order for {surface:?} at {}:{aliases}",
                            locator.root_seq
                        ))
                    })?;
                ranks.insert(rank);
            }
            if ranks.len() != 1 {
                let mut ranks = ranks.into_iter().collect::<Vec<_>>();
                ranks.sort_unstable();
                return Err(internal(format!(
                    "Physical analyzer group has conflicting lookup orders: {}",
                    ranks
                        .iter()
                        .map(u8::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )));
            }
            let rank = ranks
                .into_iter()
                .next()
                .ok_or_else(|| internal("incomplete analyzer lookup order"))?;
            ranked.push((rank, index));
        }
        ranked.sort_by_key(|(rank, index)| (*rank, *index));
        let original = values.to_vec();
        for (target, (_, source)) in ranked.into_iter().enumerate() {
            values[target] = original[source].clone();
        }
        Ok(())
    }
}

fn internal(message: impl Into<String>) -> KernelError {
    KernelError::new(ErrorCode::Internal, message)
}

fn sequence_facts(roots: &RootPayload, entry: Option<usize>) -> Result<SequenceFacts> {
    let Some(entry) = entry else {
        return Ok(SequenceFacts::default());
    };
    Ok(SequenceFacts {
        all_archived: roots.entry_archived(entry)?,
        prefer_kana: roots.entry_prefer_kana(entry)?,
        prefer_kana_on_ordinal_zero: roots.entry_prefer_kana_zero(entry)?,
    })
}

fn inflection_property(property: &MorphologyProperty) -> AnalysisInflection {
    AnalysisInflection {
        pos: property.pos.clone(),
        kind: property.kind,
        negative: property.negative,
        formal: property.formal,
        ordinal: property.ordinal,
    }
}

fn union_strings(lists: impl IntoIterator<Item = Vec<String>>) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for value in lists.into_iter().flatten() {
        if seen.insert(value.clone()) {
            result.push(value);
        }
    }
    result
}

#[derive(Hash, Eq, PartialEq)]
struct CandidateKey {
    kind: CandidateKind,
    route: Route,
    text: Vec<u16>,
    root: Option<AnalysisRoot>,
    inflection: Vec<AnalysisInflection>,
    components: Vec<ComponentKey>,
}

#[derive(Hash, Eq, PartialEq)]
struct ComponentKey {
    text: Vec<u16>,
    true_text: Option<Vec<u16>>,
    route: Route,
    reading: Vec<u16>,
    public_seq: Option<u32>,
    root: Option<AnalysisRoot>,
    inflection: Vec<AnalysisInflection>,
    primary: bool,
}

impl From<&MaterializedCandidate> for CandidateKey {
    fn from(candidate: &MaterializedCandidate) -> Self {
        Self {
            kind: candidate.kind,
            route: candidate.route,
            text: candidate.text.clone(),
            root: candidate.root.clone(),
            inflection: candidate.inflection.clone(),
            components: candidate
                .components
                .iter()
                .map(|component| ComponentKey {
                    text: component.text.clone(),
                    true_text: component.true_text.clone(),
                    route: component.route,
                    reading: component.reading.clone(),
                    public_seq: component.public_seq,
                    root: component.root.clone(),
                    inflection: component.inflection.clone(),
                    primary: component.primary,
                })
                .collect(),
        }
    }
}

fn dedupe(values: Vec<MaterializedCandidate>) -> Result<Vec<MaterializedCandidate>> {
    let mut seen = HashSet::new();
    Ok(values
        .into_iter()
        .filter(|value| seen.insert(CandidateKey::from(value)))
        .collect())
}

mod materialize;
mod merge;
mod suffixes;
pub(crate) use suffixes::SegmentSplit;
#[cfg(test)]
mod tests;
