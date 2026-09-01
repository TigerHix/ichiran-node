//! Request-local analyzer orchestration.
//!
//! This module owns candidate discovery, scoring, entity injection, and stable
//! top-N path selection. Candidate semantics stay in `AnalyzerLexicon`; public
//! token projection stays with the result layer.

use std::collections::HashMap;

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

/// Stable cross-chunk top-N state. Misc chunks do not add a word-path ref.
#[derive(Clone, Debug, PartialEq)]
pub struct AccumulatedPath {
    pub score: f64,
    pub word_paths: Vec<ChunkPathRef>,
}

impl AccumulatedPath {
    pub fn initial() -> Self {
        Self {
            score: 0.0,
            word_paths: Vec::new(),
        }
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
            let mut word_paths = prefix.word_paths.clone();
            word_paths.push(ChunkPathRef {
                chunk_index,
                path_index,
            });
            merged.insert(insertion, AccumulatedPath { score, word_paths });
            merged.truncate(limit);
        }
    }
    merged
}

fn internal(message: impl Into<String>) -> KernelError {
    KernelError::new(ErrorCode::Internal, message)
}
