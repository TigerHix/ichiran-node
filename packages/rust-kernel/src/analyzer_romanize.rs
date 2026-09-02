//! Standalone romanization of one completed analyzer result.

use crate::dto::{AnalysisChunk, AnalysisResult};
use crate::romanization::{RomanizationName, join_romanized_parts, romanize_word};

/// Romanize the same limit-one analysis shape consumed by
/// `PortableAnalyzer.romanize`.
pub fn romanize_analysis(analysis: &AnalysisResult, method: RomanizationName) -> Vec<u16> {
    if analysis.paths.is_empty() {
        return romanize_word(analysis.normalized.units(), method, None, true);
    }

    let mut parts = Vec::new();
    for chunk in &analysis.chunks {
        match chunk {
            AnalysisChunk::Misc { text, .. } => parts.push(text.units().to_vec()),
            AnalysisChunk::Word { paths, .. } => {
                let Some(path) = paths.first() else {
                    continue;
                };
                parts.extend(path.tokens.iter().map(|token| {
                    romanize_word(
                        token.reading.units(),
                        method,
                        Some(token.text.units()),
                        true,
                    )
                }));
            }
        }
    }
    join_romanized_parts(&parts)
}

#[cfg(test)]
mod tests;
