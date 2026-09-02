use crate::analyzer_engine::{AccumulatedPath, AnalyzerEngine, merge_paths};
#[cfg(target_arch = "wasm32")]
use crate::analyzer_legacy::legacy_wire_metadata;
use crate::analyzer_legacy::{
    LegacyContext, LegacyDetailedResult, LegacyDetailedSession, LegacyOptions, serialize_compact,
};
use crate::analyzer_lexicon::AnalyzerLexicon;
use crate::analyzer_model::EntityHint;
use crate::analyzer_options::{AnalyzeOptions, validate};
use crate::analyzer_projection::{ProjectionScoredCandidate, gap, project_paths, shift_token};
use crate::analyzer_romanize::romanize_analysis;
use crate::annotations::{AnalyzerAnnotations, GeneratedFacts};
use crate::characters::{BasicSplitType, basic_split, normalize};
use crate::details::{DetailRange, DetailStore};
use crate::dto::{AnalysisChunk, AnalysisPath, AnalysisResult, AnalysisToken, Utf16Text};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::{Morphology, MorphologyCandidate, Route};
use crate::pack::{Pack, PackManifest};
use crate::romanization::RomanizationName;
use crate::roots::RootPayload;
use crate::support::AnalyzerSupport;
use crate::surface::SurfaceIndex;
use crate::text::utf16;

const SURFACE_SECTION: u32 = 1;
const ROOT_SECTION: u32 = 2;
const MORPHOLOGY_SECTION: u32 = 3;
const SUPPORT_SECTION: u32 = 4;
const ANNOTATION_SECTION: u32 = 5;

#[derive(Clone, Debug, PartialEq, serde::Serialize)]
#[serde(rename_all = "camelCase")]
pub struct GeneratedLookup {
    pub candidate: MorphologyCandidate,
    pub aliases: Vec<u16>,
    pub facts: Option<GeneratedFacts>,
}

#[derive(Default)]
pub struct LegacyDetailSession {
    inner: LegacyDetailedSession,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LegacyDetailStep {
    Ready(Vec<u8>),
    Missing {
        entry_index: u32,
        range: DetailRange,
    },
}

#[cfg(target_arch = "wasm32")]
pub(crate) enum LegacyWireDetailStep {
    Ready {
        value: Vec<u8>,
        metadata: Vec<u8>,
    },
    Missing {
        entry_index: u32,
        range: DetailRange,
    },
}

enum DocumentChunk {
    Misc {
        start: usize,
        end: usize,
        text: Vec<u16>,
        token: Box<AnalysisToken>,
    },
    Word {
        start: usize,
        end: usize,
        text: Vec<u16>,
        paths: Vec<AnalysisPath>,
    },
}

/// The host-neutral owner of all resident analyzer state.
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

    pub fn entry_index_for_sequence(&self, sequence: u32) -> Result<Option<usize>> {
        self.roots.find_entry_index(sequence)
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

    /// One complete operation: UTF-16 input enters once and one result exits.
    pub fn analyze(&mut self, input: &[u16], limit: usize) -> Result<AnalysisResult> {
        self.analyze_with_options(
            input,
            &AnalyzeOptions {
                limit,
                ..AnalyzeOptions::default()
            },
        )
    }

    pub fn analyze_with_options(
        &mut self,
        input: &[u16],
        options: &AnalyzeOptions,
    ) -> Result<AnalysisResult> {
        validate(input, options)?;
        let normalized = normalize(input, false, !options.normalize_punctuation);
        let mut document = Vec::new();
        let mut accumulated = vec![AccumulatedPath::initial()];
        let mut offset = 0_usize;

        let mut lexicon = AnalyzerLexicon::new(
            &self.surface,
            &self.roots,
            &self.morphology,
            &self.support,
            &mut self.annotations,
        );
        lexicon.reset();
        let mut engine = AnalyzerEngine::new(&self.surface, &self.support, &mut lexicon);

        for segment in basic_split(&normalized) {
            let start = offset;
            let end = start.checked_add(segment.text.len()).ok_or_else(|| {
                KernelError::new(ErrorCode::Internal, "analysis chunk offset overflow")
            })?;
            match segment.kind {
                BasicSplitType::Misc => {
                    let token = gap(&normalized, start, end)?;
                    document.push(DocumentChunk::Misc {
                        start,
                        end,
                        text: segment.text,
                        token: Box::new(token),
                    });
                }
                BasicSplitType::Word => {
                    let entities = local_entities(&options.entities, start, end);
                    let analysis = engine.analyze_word(&segment.text, options.limit, &entities)?;
                    let projected_candidates = analysis
                        .candidates
                        .into_iter()
                        .map(|(id, value)| {
                            (
                                id,
                                ProjectionScoredCandidate {
                                    candidate: value.candidate,
                                    info: value.info,
                                },
                            )
                        })
                        .collect();
                    let mut paths = project_paths(
                        &segment.text,
                        &analysis.paths,
                        &projected_candidates,
                        &entities,
                    )?;
                    for path in &mut paths {
                        for token in &mut path.tokens {
                            *token = shift_token(token.clone(), start)?;
                        }
                    }
                    let chunk_index = document.len();
                    accumulated =
                        merge_paths(&accumulated, &analysis.paths, chunk_index, options.limit);
                    document.push(DocumentChunk::Word {
                        start,
                        end,
                        text: segment.text,
                        paths,
                    });
                }
            }
            offset = end;
        }

        let paths = materialize_document_paths(&document, &accumulated)?;
        let chunks = document.into_iter().map(public_chunk).collect();
        Ok(AnalysisResult {
            input: Utf16Text::from_units(input),
            normalized: Utf16Text::from_units(&normalized),
            compute_ms: 0.0,
            chunks,
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

    pub fn analyze_json_with_options(
        &mut self,
        input: &[u16],
        options: &AnalyzeOptions,
    ) -> Result<Vec<u8>> {
        serde_json::to_vec(&self.analyze_with_options(input, options)?)
            .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))
    }

    pub fn romanize_with_options(
        &mut self,
        input: &[u16],
        options: &AnalyzeOptions,
        method: RomanizationName,
    ) -> Result<Vec<u16>> {
        let analysis = self.analyze_with_options(
            input,
            &AnalyzeOptions {
                limit: 1,
                entities: options.entities.clone(),
                normalize_punctuation: options.normalize_punctuation,
            },
        )?;
        Ok(romanize_analysis(&analysis, method))
    }

    pub fn serialize_legacy_compact_json(
        &self,
        result: &AnalysisResult,
        method: Option<RomanizationName>,
    ) -> Result<Vec<u8>> {
        let value = serialize_compact(
            result,
            &LegacyOptions {
                method,
                ..LegacyOptions::default()
            },
        );
        serde_json::to_vec(&value)
            .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))
    }

    pub fn serialize_legacy_detailed_json(
        &mut self,
        session: &mut LegacyDetailSession,
        result: &AnalysisResult,
        details: &DetailStore,
        method: Option<RomanizationName>,
    ) -> Result<LegacyDetailStep> {
        match self.serialize_legacy_detailed(session, result, details, method)? {
            LegacyDetailedResult::Ready(value) => serde_json::to_vec(&value)
                .map(LegacyDetailStep::Ready)
                .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string())),
            LegacyDetailedResult::MissingDetail(request) => Ok(LegacyDetailStep::Missing {
                entry_index: request.entry_index,
                range: request.range,
            }),
        }
    }

    #[cfg(target_arch = "wasm32")]
    pub(crate) fn serialize_legacy_detailed_wire_json(
        &mut self,
        session: &mut LegacyDetailSession,
        result: &AnalysisResult,
        details: &DetailStore,
        method: Option<RomanizationName>,
    ) -> Result<LegacyWireDetailStep> {
        match self.serialize_legacy_detailed(session, result, details, method)? {
            LegacyDetailedResult::Ready(value) => {
                let metadata = serde_json::to_vec(&legacy_wire_metadata(&value))
                    .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))?;
                let value = serde_json::to_vec(&value)
                    .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))?;
                Ok(LegacyWireDetailStep::Ready { value, metadata })
            }
            LegacyDetailedResult::MissingDetail(request) => Ok(LegacyWireDetailStep::Missing {
                entry_index: request.entry_index,
                range: request.range,
            }),
        }
    }

    fn serialize_legacy_detailed(
        &mut self,
        session: &mut LegacyDetailSession,
        result: &AnalysisResult,
        details: &DetailStore,
        method: Option<RomanizationName>,
    ) -> Result<LegacyDetailedResult> {
        let mut context = LegacyContext {
            roots: &self.roots,
            support: &self.support,
            surface: &self.surface,
            annotations: &mut self.annotations,
        };
        let options = LegacyOptions {
            method,
            ..LegacyOptions::default()
        };
        session
            .inner
            .serialize(result, details, &mut context, &options)
    }
}

fn local_entities(entities: &[EntityHint], start: usize, end: usize) -> Vec<EntityHint> {
    entities
        .iter()
        .filter(|entity| entity.start >= start && entity.end <= end)
        .map(|entity| EntityHint {
            start: entity.start - start,
            end: entity.end - start,
            boost: entity.boost,
        })
        .collect()
}

fn materialize_document_paths(
    chunks: &[DocumentChunk],
    accumulated: &[AccumulatedPath],
) -> Result<Vec<AnalysisPath>> {
    accumulated
        .iter()
        .map(|path| {
            let mut tokens = Vec::new();
            let mut word = 0;
            let word_paths = path.word_paths();
            for (chunk_index, chunk) in chunks.iter().enumerate() {
                match chunk {
                    DocumentChunk::Misc { token, .. } => tokens.push((**token).clone()),
                    DocumentChunk::Word { paths, .. } => {
                        let reference = word_paths.get(word).ok_or_else(|| {
                            KernelError::new(
                                ErrorCode::Internal,
                                "accumulated path is missing a word chunk",
                            )
                        })?;
                        if reference.chunk_index != chunk_index {
                            return Err(KernelError::new(
                                ErrorCode::Internal,
                                "accumulated word path is out of document order",
                            ));
                        }
                        let selected = paths.get(reference.path_index).ok_or_else(|| {
                            KernelError::new(
                                ErrorCode::Internal,
                                "accumulated word path index is out of range",
                            )
                        })?;
                        tokens.extend(selected.tokens.iter().cloned());
                        word += 1;
                    }
                }
            }
            if word != word_paths.len() {
                return Err(KernelError::new(
                    ErrorCode::Internal,
                    "accumulated path has extra word chunks",
                ));
            }
            Ok(AnalysisPath {
                score: path.score,
                tokens,
            })
        })
        .collect()
}

fn public_chunk(chunk: DocumentChunk) -> AnalysisChunk {
    match chunk {
        DocumentChunk::Misc {
            start, end, text, ..
        } => AnalysisChunk::Misc {
            start,
            end,
            text: Utf16Text::from_units(&text),
        },
        DocumentChunk::Word {
            start,
            end,
            text,
            paths,
        } => AnalysisChunk::Word {
            start,
            end,
            text: Utf16Text::from_units(&text),
            paths,
        },
    }
}
