//! Request-local analyzer orchestration.
//!
//! This module owns candidate discovery, scoring, entity injection, and stable
//! top-N path selection. Candidate semantics stay in `AnalyzerLexicon`; public
//! token projection stays with the result layer.

use std::collections::HashMap;
use std::sync::Arc;

use crate::analyzer_lexicon::{AnalyzerLexicon, MaterializedCandidate};
use crate::analyzer_model::{EntityHint, PathResult, ScoreInfo, SegmentGroup};
use crate::analyzer_paths::{add_entity_groups, find_paths};
use crate::error::{ErrorCode, KernelError, Result};
use crate::support::AnalyzerSupport;
use crate::surface::SurfaceIndex;

mod groups;
#[cfg(test)]
mod tests;

const MAX_WORD_CODE_UNITS: usize = 50;

#[derive(Clone, Debug, PartialEq)]
pub struct EngineCandidate {
    pub candidate: MaterializedCandidate,
    pub score: f64,
    pub info: ScoreInfo,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WordAnalysis {
    pub groups: Vec<SegmentGroup>,
    pub candidates: HashMap<i64, EngineCandidate>,
    pub paths: Vec<PathResult>,
}

/// One selected word-path rank within a projected document chunk.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ChunkPathRef {
    pub chunk_index: usize,
    pub path_index: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct ChunkPathNode {
    reference: ChunkPathRef,
    previous: Option<Arc<ChunkPathNode>>,
}

/// Stable cross-chunk top-N state. Misc chunks do not add a word-path ref.
#[derive(Clone, Debug, PartialEq)]
pub struct AccumulatedPath {
    pub score: f64,
    tail: Option<Arc<ChunkPathNode>>,
    word_count: usize,
}

impl AccumulatedPath {
    pub fn initial() -> Self {
        Self {
            score: 0.0,
            tail: None,
            word_count: 0,
        }
    }

    fn appended(&self, score: f64, reference: ChunkPathRef) -> Self {
        Self {
            score,
            tail: Some(Arc::new(ChunkPathNode {
                reference,
                previous: self.tail.clone(),
            })),
            word_count: self.word_count + 1,
        }
    }

    pub(crate) fn word_paths(&self) -> Vec<ChunkPathRef> {
        let mut paths = Vec::with_capacity(self.word_count);
        let mut current = self.tail.as_deref();
        while let Some(node) = current {
            paths.push(node.reference);
            current = node.previous.as_deref();
        }
        paths.reverse();
        paths
    }
}

pub struct AnalyzerEngine<'a, 'lexicon> {
    surface: &'a SurfaceIndex,
    support: &'a AnalyzerSupport,
    lexicon: &'a mut AnalyzerLexicon<'lexicon>,
}

impl<'a, 'lexicon> AnalyzerEngine<'a, 'lexicon> {
    pub fn new(
        surface: &'a SurfaceIndex,
        support: &'a AnalyzerSupport,
        lexicon: &'a mut AnalyzerLexicon<'lexicon>,
    ) -> Self {
        Self {
            surface,
            support,
            lexicon,
        }
    }

    pub fn analyze_word(
        &mut self,
        text: &[u16],
        limit: usize,
        entities: &[EntityHint],
    ) -> Result<WordAnalysis> {
        let (groups, candidates) = self.groups(text)?;
        let groups = add_entity_groups(&groups, entities, text);
        let paths = find_paths(&groups, text.len(), limit, entities)
            .map_err(|message| KernelError::new(ErrorCode::InvalidInput, message))?;
        Ok(WordAnalysis {
            groups,
            candidates,
            paths,
        })
    }
}

/// Stable Cartesian top-N merge used after each projected word chunk.
pub fn merge_paths(
    left: &[AccumulatedPath],
    right: &[PathResult],
    chunk_index: usize,
    limit: usize,
) -> Vec<AccumulatedPath> {
    if limit == 0 {
        return Vec::new();
    }
    let mut merged = Vec::new();
    for prefix in left {
        for (path_index, suffix) in right.iter().enumerate() {
            let score = prefix.score + suffix.score;
            let insertion =
                merged.partition_point(|existing: &AccumulatedPath| existing.score >= score);
            if insertion >= limit {
                continue;
            }
            merged.insert(
                insertion,
                prefix.appended(
                    score,
                    ChunkPathRef {
                        chunk_index,
                        path_index,
                    },
                ),
            );
            merged.truncate(limit);
        }
    }
    merged
}

fn internal(message: impl Into<String>) -> KernelError {
    KernelError::new(ErrorCode::Internal, message)
}
