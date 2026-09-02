//! Request-local suffix recursion owned by `AnalyzerLexicon`.

mod rules;
mod selection;
mod splits;

pub(crate) use splits::SegmentSplit;

use super::*;
use crate::analyzer_suffixes::unique_suffix;

impl AnalyzerLexicon<'_> {
    /// Direct lexical rows plus recursively materialized suffix rows.
    #[cfg(test)]
    pub fn full(&mut self, surface: &[u16]) -> Result<Vec<MaterializedCandidate>> {
        self.full_at(surface, 0)
    }

    pub(super) fn full_at(
        &mut self,
        surface: &[u16],
        depth: usize,
    ) -> Result<Vec<MaterializedCandidate>> {
        let direct = self.lexical(surface)?;
        let suffixes = self.suffixes(surface, &direct, depth)?;
        let mut result = direct;
        result.extend(suffixes);
        dedupe(result)
    }

    pub fn suffixes(
        &mut self,
        surface: &[u16],
        direct_matches: &[MaterializedCandidate],
        depth: usize,
    ) -> Result<Vec<MaterializedCandidate>> {
        if direct_matches.is_empty()
            && let Some(cached) = self.full_cache.get(surface)
        {
            return Ok(cached.clone());
        }
        let mut result = Vec::new();
        for matched in self.support.suffix_matches_ending_at(
            surface,
            surface.len(),
            surface.len().saturating_sub(1),
        )? {
            if matched.start == 0 {
                continue;
            }
            let root = &surface[..matched.start];
            let suffix = utf16(&matched.text);
            for value in matched.values {
                let suffix_class = value.form.as_ref().map_or_else(
                    || Ok(value.keyword.clone()),
                    |form| {
                        Ok(self
                            .support
                            .suffix_class(form.seq)?
                            .unwrap_or_else(|| value.keyword.clone()))
                    },
                )?;
                if !direct_matches.is_empty() && unique_suffix(&suffix_class, direct_matches) {
                    continue;
                }
                result.extend(self.apply_suffix(
                    &value.keyword,
                    root,
                    &suffix,
                    value.form.as_ref(),
                    surface,
                    depth,
                )?);
            }
        }
        let result = dedupe(result)?;
        if direct_matches.is_empty() {
            self.full_cache.insert(surface.to_vec(), result.clone());
        }
        Ok(result)
    }

    pub fn with_suru_break(&mut self, candidate: &ScoreCandidate) -> Result<ScoreCandidate> {
        let ScoreCandidate::Word(word) = candidate else {
            return Ok(candidate.clone());
        };
        if word.suru_break.is_some()
            || (!word.positions.iter().any(|value| value == "vs-s")
                && !word.positions.iter().any(|value| value == "v5s"))
        {
            return Ok(candidate.clone());
        }
        let end = word.text.len();
        let matches =
            self.support
                .suffix_matches_ending_at(&word.text, end, end.saturating_sub(1))?;
        for matched in matches.iter().rev() {
            for value in &matched.values {
                if value.keyword != ":suru" {
                    continue;
                }
                let Some(form) = &value.form else {
                    continue;
                };
                let Some(suffix) = self.suffix_component(form)? else {
                    return Ok(candidate.clone());
                };
                let mut result = word.clone();
                result.suru_break = Some(crate::analyzer_model::SuruBreakFacts {
                    suffix_text: utf16(&matched.text),
                    candidate: Box::new(suffix.score_facts),
                });
                return Ok(ScoreCandidate::Word(result));
            }
        }
        Ok(candidate.clone())
    }
}
