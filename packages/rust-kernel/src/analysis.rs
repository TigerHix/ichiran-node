use crate::annotations::{AnalyzerAnnotations, GeneratedFacts};
use crate::dto::{
    AnalysisAlternative, AnalysisChunk, AnalysisPath, AnalysisResult, AnalysisRoot, AnalysisToken,
    PublicRoute, Utf16Text,
};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::{Morphology, MorphologyCandidate, MorphologyProperty, Route};
use crate::pack::{Pack, PackManifest};
use crate::romanization::romanize;
use crate::roots::RootPayload;
use crate::scoring::{WordFacts, score_word};
use crate::support::AnalyzerSupport;
use crate::surface::SurfaceIndex;
use crate::text::{string as utf16_string, utf16};

const SURFACE_SECTION: u32 = 1;
const ROOT_SECTION: u32 = 2;
const MORPHOLOGY_SECTION: u32 = 3;
const SUPPORT_SECTION: u32 = 4;
const ANNOTATION_SECTION: u32 = 5;
const SCORE_CUTOFF: i32 = 5;

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GeneratedLookup {
    pub candidate: MorphologyCandidate,
    pub aliases: Vec<u16>,
    pub facts: Option<GeneratedFacts>,
}

struct Candidate {
    id: u32,
    text: String,
    route: Route,
    reading: String,
    pos: Vec<String>,
    score: i32,
    archived_score: i32,
    archived: bool,
    physical_group: Option<u32>,
    member_ord: Option<u8>,
    common: Option<u8>,
    entry_index: Option<usize>,
    root: AnalysisRoot,
    inflection: Vec<MorphologyProperty>,
}

struct CandidateGroup {
    start: usize,
    end: usize,
    matches: usize,
    candidates: Vec<Candidate>,
}

/// The host-neutral owner of all hot analyzer state.
pub struct Kernel {
    pack: Pack,
    surface: SurfaceIndex,
    roots: RootPayload,
    morphology: Morphology,
    support: AnalyzerSupport,
    annotations: AnalyzerAnnotations,
}

impl Kernel {
    pub fn open(hot: Vec<u8>) -> Result<Self> {
        let pack = Pack::open(hot)?;
        pack.verify_all()?;
        let surface = SurfaceIndex::open(pack.section_data(SURFACE_SECTION)?)?;
        let roots = RootPayload::open(pack.section_data(ROOT_SECTION)?)?;
        let morphology = Morphology::open(pack.section_data(MORPHOLOGY_SECTION)?)?;
        let support = AnalyzerSupport::open(pack.section_data(SUPPORT_SECTION)?)?;
        let annotations = AnalyzerAnnotations::open(pack.section_data(ANNOTATION_SECTION)?)?;
        if surface.direct_count() as usize != roots.surface_count {
            return Err(KernelError::new(
                ErrorCode::CorruptPayload,
                "surface and root-payload direct counts disagree",
            ));
        }
        Ok(Self {
            pack,
            surface,
            roots,
            morphology,
            support,
            annotations,
        })
    }

    pub fn manifest(&self) -> &PackManifest {
        self.pack.manifest()
    }

    pub fn surface(&self) -> &SurfaceIndex {
        &self.surface
    }

    pub fn generated_decoded_bytes(&self) -> usize {
        self.annotations.decoded_bytes()
    }

    pub fn generated_decoded_block_count(&self) -> usize {
        self.annotations.decoded_block_count()
    }

    pub fn generated_block_count(&self) -> usize {
        self.annotations.generated_block_count()
    }

    pub fn preload_all_generated(&mut self) -> Result<()> {
        self.annotations.preload_all_generated()
    }

    pub fn resident_payload_bytes(&self) -> usize {
        self.pack.manifest().byte_length + self.annotations.decoded_bytes()
    }

    pub fn generated_lookup(
        &mut self,
        surface: &[u16],
        route: Route,
    ) -> Result<Vec<GeneratedLookup>> {
        let candidates = self.morphology.lookup(surface, route)?;
        let mut generated = Vec::with_capacity(candidates.len());
        for candidate in candidates {
            let aliases = self.support.generated_aliases(&candidate.rule_ids)?;
            let facts = self.annotations.generated(candidate.root_seq, &aliases)?;
            generated.push(GeneratedLookup {
                candidate,
                aliases,
                facts,
            });
        }
        Ok(generated)
    }

    /// One complete operation: UTF-16 input enters once and one serialized DTO exits.
    pub fn analyze(&mut self, input: &[u16], limit: usize) -> Result<AnalysisResult> {
        if limit == 0 {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                "analysis limit must be positive",
            ));
        }
        let text = Utf16Text::from_units(input);
        let groups = self.groups(input)?;
        let Some(group) = groups.into_iter().find(|group| {
            group.start == 0 && group.end == input.len() && !group.candidates.is_empty()
        }) else {
            let token = gap_token(&text, 0, input.len());
            let path = AnalysisPath {
                score: 0,
                tokens: vec![token],
            };
            return Ok(AnalysisResult {
                input: text.clone(),
                normalized: text.clone(),
                compute_ms: 0,
                chunks: vec![AnalysisChunk::Misc {
                    start: 0,
                    end: input.len(),
                    text,
                }],
                paths: vec![path],
            });
        };
        let paths = group_paths(group, limit);
        Ok(AnalysisResult {
            input: text.clone(),
            normalized: text.clone(),
            compute_ms: 0,
            chunks: vec![AnalysisChunk::Word {
                start: 0,
                end: input.len(),
                text,
                paths: paths.clone(),
            }],
            paths,
        })
    }

    pub fn analyze_str(&mut self, input: &str, limit: usize) -> Result<AnalysisResult> {
        self.analyze(&utf16(input), limit)
    }

    pub fn analyze_json(&mut self, input: &[u16], limit: usize) -> Result<Vec<u8>> {
        serde_json::to_vec(&self.analyze(input, limit)?)
            .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))
    }

    fn groups(&mut self, input: &[u16]) -> Result<Vec<CandidateGroup>> {
        let mut groups = Vec::new();
        let mut next_id = 1_u32;
        for start in 0..input.len() {
            for matched in self.surface.scan(input, start, 50)? {
                let surface = &input[start..matched.end];
                let mut candidates = Vec::new();
                if matched.direct {
                    let rank = matched.direct_rank.ok_or_else(|| {
                        KernelError::new(ErrorCode::CorruptPayload, "direct match has no rank")
                    })?;
                    let first = self.roots.surface_form_start(rank)?;
                    let count = self.roots.surface_form_count(rank)?;
                    for form in first..first + count {
                        candidates.push(self.direct_candidate(surface, form)?);
                    }
                }
                if matched.morphology {
                    for value in self.morphology.lookup(surface, matched.route)? {
                        candidates.push(self.morphology_candidate(value)?);
                    }
                }
                if candidates.is_empty() {
                    continue;
                }
                candidates = merge_physical(candidates);
                for candidate in &mut candidates {
                    candidate.id = next_id;
                    next_id = next_id.checked_add(1).ok_or_else(|| {
                        KernelError::new(ErrorCode::OutOfRange, "candidate ID overflow")
                    })?;
                }
                let matches = candidates.len();
                candidates.retain(|candidate| candidate.score >= SCORE_CUTOFF);
                stable_score_sort(&mut candidates);
                if let Some(best) = candidates.first().map(|value| value.score) {
                    candidates.retain(|candidate| candidate.score * 2 >= best);
                }
                if !candidates.is_empty() {
                    groups.push(CandidateGroup {
                        start,
                        end: matched.end,
                        matches,
                        candidates,
                    });
                }
            }
        }
        Ok(groups)
    }

    fn direct_candidate(&self, surface: &[u16], form: usize) -> Result<Candidate> {
        let text = utf16_string(surface, "direct surface")?;
        let entry = self.roots.form_entry_index(form)?;
        let seq = self.roots.entry_seq(entry)?;
        let route = self.roots.form_route(form)?;
        let best = self
            .roots
            .resolve_surface_reference(self.roots.form_best_reference(form)?, |rank| {
                self.surface.direct_surface(rank)
            })?;
        let root = match route {
            Route::Kanji => AnalysisRoot {
                seq,
                form: text.clone(),
                reading: best.unwrap_or_else(|| text.clone()),
            },
            Route::Kana => AnalysisRoot {
                seq,
                form: best.unwrap_or_else(|| text.clone()),
                reading: text.clone(),
            },
        };
        let positions = self.roots.entry_positions(entry)?;
        let common = self.roots.form_common(form)?;
        let archived = self.roots.entry_archived(entry)?;
        let facts = WordFacts {
            text: surface,
            route,
            seq,
            ord: self.roots.form_ordinal(form)?,
            common,
            nokanji: self.roots.form_nokanji(form)?,
            root: true,
            n_kanji: self.roots.entry_n_kanji(entry)?,
            primary_nokanji: self.roots.entry_primary_nokanji(entry)?,
            conjugations: &[],
            positions: &positions,
            archived,
            prefer_kana: self.roots.entry_prefer_kana(entry)?,
            prefer_kana_zero: self.roots.entry_prefer_kana_zero(entry)?,
            inherited_common: None,
        };
        let score = score_word(facts);
        Ok(Candidate {
            id: 0,
            text,
            route,
            reading: root.reading.clone(),
            pos: positions,
            score,
            archived_score: score,
            archived,
            physical_group: None,
            member_ord: None,
            common,
            entry_index: Some(entry),
            root,
            inflection: Vec::new(),
        })
    }

    fn morphology_candidate(&mut self, value: MorphologyCandidate) -> Result<Candidate> {
        let entry = self.roots.find_entry_index(value.root_seq)?;
        let aliases = self.support.generated_aliases(&value.rule_ids)?;
        let generated = self.annotations.generated(value.root_seq, &aliases)?;
        let mut inflection = value.path.clone();
        if let (Some(last), Some(member)) = (
            inflection.last_mut(),
            generated
                .as_ref()
                .and_then(|facts| facts.members.as_ref())
                .and_then(|members| members.first()),
        ) {
            last.pos = self
                .morphology
                .position(member.property.pos_id as usize)?
                .to_owned();
            last.kind = member.property.kind;
            last.negative = member.property.negative;
            last.formal = member.property.formal;
        }
        let mut positions =
            entry.map_or(Ok(Vec::new()), |index| self.roots.entry_positions(index))?;
        for property in &inflection {
            if !positions.contains(&property.pos) {
                positions.push(property.pos.clone());
            }
        }
        let archived = entry.map_or(Ok(false), |index| self.roots.entry_archived(index))?;
        let prefer_kana = entry.map_or(Ok(false), |index| self.roots.entry_prefer_kana(index))?;
        let prefer_kana_zero =
            entry.map_or(Ok(false), |index| self.roots.entry_prefer_kana_zero(index))?;
        let n_kanji = generated
            .as_ref()
            .and_then(|facts| facts.n_kanji)
            .map_or_else(
                || entry.map_or(Ok(0), |index| self.roots.entry_n_kanji(index)),
                Ok,
            )?;
        let text_units = utf16(&value.surface);
        let facts = WordFacts {
            text: &text_units,
            route: value.route,
            seq: value.root_seq,
            ord: value.ord,
            common: None,
            nokanji: value.route == Route::Kana && value.source_form == value.source_reading,
            root: false,
            n_kanji,
            primary_nokanji: false,
            conjugations: &inflection,
            positions: &positions,
            archived,
            prefer_kana,
            prefer_kana_zero,
            inherited_common: value.common,
        };
        let score = score_word(facts);
        let archived_score = score_word(WordFacts {
            text: &text_units,
            route: value.route,
            seq: value.root_seq,
            ord: value.ord,
            common: None,
            nokanji: value.route == Route::Kana && value.source_form == value.source_reading,
            root: false,
            n_kanji,
            primary_nokanji: false,
            conjugations: &inflection,
            positions: &positions,
            archived: true,
            prefer_kana,
            prefer_kana_zero,
            inherited_common: value.common,
        });
        let physical_group = generated.as_ref().and_then(|facts| facts.physical_group);
        let member_ord = generated
            .as_ref()
            .and_then(|facts| facts.members.as_ref())
            .and_then(|members| members.first())
            .map(|member| member.member_ord);
        Ok(Candidate {
            id: 0,
            text: value.surface,
            route: value.route,
            reading: value.reading,
            pos: positions,
            score,
            archived_score,
            archived,
            physical_group,
            member_ord,
            common: value.common,
            entry_index: entry,
            root: AnalysisRoot {
                seq: value.root_seq,
                form: value.source_form,
                reading: value.source_reading,
            },
            inflection,
        })
    }
}

fn merge_physical(candidates: Vec<Candidate>) -> Vec<Candidate> {
    let mut merged: Vec<Candidate> = Vec::new();
    for mut candidate in candidates {
        let Some(group) = candidate.physical_group else {
            merged.push(candidate);
            continue;
        };
        let Some(index) = merged
            .iter()
            .position(|value| value.physical_group == Some(group))
        else {
            merged.push(candidate);
            continue;
        };
        let existing = &mut merged[index];
        let archived = existing.archived || candidate.archived;
        if candidate.member_ord.unwrap_or(u8::MAX) < existing.member_ord.unwrap_or(u8::MAX) {
            candidate.archived = archived;
            if archived {
                candidate.score = candidate.archived_score;
            }
            *existing = candidate;
        } else {
            existing.archived = archived;
            if archived {
                existing.score = existing.archived_score;
            }
        }
    }
    merged
}

fn group_paths(group: CandidateGroup, limit: usize) -> Vec<AnalysisPath> {
    group
        .candidates
        .into_iter()
        .take(limit)
        .map(|candidate| {
            let alternatives = Vec::new();
            let mut token = token_from_candidate(
                candidate,
                group.start,
                group.end,
                alternatives,
                group.matches,
            );
            token.alternatives = vec![alternative_from_token(&token)];
            AnalysisPath {
                score: token.score,
                tokens: vec![token],
            }
        })
        .collect()
}

fn token_from_candidate(
    candidate: Candidate,
    start: usize,
    end: usize,
    alternatives: Vec<AnalysisAlternative>,
    matches: usize,
) -> AnalysisToken {
    AnalysisToken {
        candidate_id: Some(candidate.id),
        start,
        end,
        text: candidate.text.into(),
        true_text: None,
        route: candidate.route.into(),
        romanized: romanize(&candidate.reading).into(),
        reading: candidate.reading.into(),
        pos: candidate.pos,
        score: candidate.score,
        entry_index: candidate.entry_index,
        root: Some(candidate.root),
        inflection: candidate.inflection,
        components: Vec::new(),
        skipped: matches.saturating_sub(alternatives.len() + 1),
        alternatives,
        entity: false,
        counter: None,
    }
}

fn alternative_from_token(token: &AnalysisToken) -> AnalysisAlternative {
    AnalysisAlternative {
        candidate_id: token.candidate_id.unwrap_or_default(),
        text: token.text.clone(),
        true_text: token.true_text.clone(),
        route: match token.route {
            PublicRoute::Kana => Route::Kana,
            PublicRoute::Kanji => Route::Kanji,
            PublicRoute::Gap => unreachable!("gap tokens do not have alternatives"),
        },
        reading: token.reading.clone(),
        romanized: token.romanized.clone(),
        pos: token.pos.clone(),
        score: token.score,
        entry_index: token.entry_index,
        root: token.root.clone(),
        inflection: token.inflection.clone(),
        components: Vec::new(),
        counter: None,
    }
}

fn gap_token(text: &Utf16Text, start: usize, end: usize) -> AnalysisToken {
    AnalysisToken {
        candidate_id: None,
        start,
        end,
        text: text.clone(),
        true_text: None,
        route: PublicRoute::Gap,
        reading: text.clone(),
        romanized: text.clone(),
        pos: Vec::new(),
        score: 0,
        entry_index: None,
        root: None,
        inflection: Vec::new(),
        components: Vec::new(),
        alternatives: Vec::new(),
        skipped: 0,
        entity: false,
        counter: None,
    }
}

fn stable_score_sort(candidates: &mut [Candidate]) {
    candidates.sort_by(|left, right| {
        right
            .score
            .cmp(&left.score)
            .then_with(|| common_order(left.common, right.common))
    });
}

fn common_order(left: Option<u8>, right: Option<u8>) -> std::cmp::Ordering {
    match (left, right) {
        (Some(0), Some(0)) | (None, None) => std::cmp::Ordering::Equal,
        (Some(0), _) => std::cmp::Ordering::Less,
        (_, Some(0)) => std::cmp::Ordering::Greater,
        (Some(left), Some(right)) => left.cmp(&right),
        (Some(_), None) => std::cmp::Ordering::Less,
        (None, Some(_)) => std::cmp::Ordering::Greater,
    }
}
