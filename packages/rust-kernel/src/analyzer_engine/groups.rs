use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use super::*;
use crate::analyzer_lexicon::{CandidateKind, MaterializedCandidate, SegmentSplit};
use crate::analyzer_model::{RuleWordKind, ScoreOptions, Segment, SegmentGroup, SegmentRuleFacts};
use crate::analyzer_scoring::{filter_and_cull_segments, score_candidate};
use crate::analyzer_suffixes::unique_suffix;
use crate::characters::{
    CharClass, ITERATION_CHARACTERS, KANA_CHARACTERS, MODIFIER_CHARACTERS, consecutive_char_groups,
    long_vowel_modifier_p, sequential_kanji_positions,
};
use crate::text::utf16;

const FORCE_KANJI_BREAK: &[u16] = &[0x3067, 0x3059]; // です
const NO_KANJI_BREAK: &[u16] = &[0x65e5, 0x7f6e]; // 日置

struct RawGroup {
    start: usize,
    end: usize,
    candidates: Vec<MaterializedCandidate>,
}

/// JavaScript `Map` replacement: overwriting an end keeps its insertion slot.
pub(super) struct EndCandidates(Vec<(usize, Vec<MaterializedCandidate>)>);

impl EndCandidates {
    pub(super) fn new() -> Self {
        Self(Vec::new())
    }

    fn get(&self, end: usize) -> &[MaterializedCandidate] {
        self.0
            .iter()
            .find(|(existing, _)| *existing == end)
            .map_or(&[], |(_, values)| values.as_slice())
    }

    pub(super) fn set(&mut self, end: usize, values: Vec<MaterializedCandidate>) {
        if let Some((_, existing)) = self.0.iter_mut().find(|(existing, _)| *existing == end) {
            *existing = values;
        } else {
            self.0.push((end, values));
        }
    }

    pub(super) fn append(&mut self, end: usize, mut values: Vec<MaterializedCandidate>) {
        if let Some((_, existing)) = self.0.iter_mut().find(|(existing, _)| *existing == end) {
            existing.append(&mut values);
        } else {
            self.0.push((end, values));
        }
    }
}

impl IntoIterator for EndCandidates {
    type Item = (usize, Vec<MaterializedCandidate>);
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl AnalyzerEngine<'_, '_> {
    pub fn groups(
        &mut self,
        text: &[u16],
    ) -> Result<(Vec<SegmentGroup>, HashMap<i64, EngineCandidate>)> {
        let sticky = sticky_positions(text);
        let katakana_ends = group_ends(CharClass::Katakana, text);
        let number_ends = group_ends(CharClass::Number, text);
        let suffixes_by_start = self.distribute_suffixes(text)?;
        let mut raw = Vec::new();
        let mut reachable_ends = HashSet::new();
        let mut kanji_break = HashSet::new();

        for start in 0..text.len() {
            if sticky.contains(&start) {
                continue;
            }
            let mut by_end = EndCandidates::new();
            for matched in self.surface.scan(text, start, MAX_WORD_CODE_UNITS)? {
                if sticky.contains(&matched.end) {
                    continue;
                }
                let values = self.lexicon.lexical(&text[start..matched.end])?;
                if !values.is_empty() {
                    by_end.set(matched.end, values);
                }
            }

            let max_end = text.len().min(start.saturating_add(MAX_WORD_CODE_UNITS));
            let mut suffix_candidates = EndCandidates::new();
            for matched in &suffixes_by_start[start] {
                if sticky.contains(&matched.end) {
                    continue;
                }
                let direct = by_end.get(matched.end);
                let root = &text[start..matched.start];
                let surface = &text[start..matched.end];
                let suffix = utf16(&matched.text);
                for value in &matched.values {
                    let suffix_class = if let Some(form) = &value.form {
                        self.support
                            .suffix_class(form.seq)?
                            .unwrap_or_else(|| value.keyword.clone())
                    } else {
                        value.keyword.clone()
                    };
                    if !direct.is_empty() && unique_suffix(&suffix_class, direct) {
                        continue;
                    }
                    suffix_candidates.append(
                        matched.end,
                        self.lexicon.apply_suffix(
                            &value.keyword,
                            root,
                            &suffix,
                            value.form.as_ref(),
                            surface,
                            0,
                        )?,
                    );
                }
            }
            for (end, suffixes) in suffix_candidates {
                if suffixes.is_empty() {
                    continue;
                }
                let mut values = by_end.get(end).to_vec();
                values.extend(suffixes);
                by_end.set(end, self.lexicon.dedupe_candidates(values)?);
            }

            if let Some(&katakana_end) = katakana_ends.get(&start)
                && katakana_end <= max_end
                && !sticky.contains(&katakana_end)
            {
                let existing = by_end.get(katakana_end).to_vec();
                let simple = existing
                    .iter()
                    .filter(|value| value.kind == CandidateKind::Simple)
                    .cloned()
                    .collect::<Vec<_>>();
                let proxies = self
                    .lexicon
                    .katakana_proxy(&text[start..katakana_end], &simple)?;
                if proxies.is_empty() {
                    if !existing.is_empty() {
                        by_end.set(katakana_end, existing);
                    }
                } else {
                    let mut values = existing;
                    values.extend(proxies);
                    by_end.set(katakana_end, self.lexicon.dedupe_candidates(values)?);
                }
            }

            if let Some(&number_end) = number_ends.get(&start) {
                let number_text = &text[start..number_end];
                if !sticky.contains(&number_end) {
                    let direct = by_end.get(number_end).to_vec();
                    let variants = self.support.counters(&[])?;
                    let mut numbers = self.lexicon.counter_candidates(
                        number_text,
                        &[],
                        direct.is_empty(),
                        &variants,
                    )?;
                    if numbers.is_empty() {
                        if !direct.is_empty() {
                            by_end.set(number_end, direct);
                        }
                    } else {
                        let mut values = direct;
                        values.append(&mut numbers);
                        by_end.set(number_end, values);
                    }
                }
                for matched in self.support.counter_matches_starting_at(
                    text,
                    number_end,
                    max_end.saturating_sub(number_end),
                )? {
                    if sticky.contains(&matched.end) {
                        continue;
                    }
                    let direct = by_end.get(matched.end).to_vec();
                    let mut counters = self.lexicon.counter_candidates(
                        number_text,
                        &utf16(&matched.text),
                        direct.is_empty(),
                        &matched.values,
                    )?;
                    if counters.is_empty() {
                        if !direct.is_empty() {
                            by_end.set(matched.end, direct);
                        }
                    } else {
                        let mut values = direct;
                        values.append(&mut counters);
                        by_end.set(matched.end, values);
                    }
                }
            }

            for (end, values) in by_end {
                if values.is_empty() {
                    continue;
                }
                if start == 0 || reachable_ends.contains(&start) {
                    record_kanji_breaks(&text[start..end], start, end, &mut kanji_break);
                }
                reachable_ends.insert(end);
                raw.push(RawGroup {
                    start,
                    end,
                    candidates: self.lexicon.dedupe_candidates(values)?,
                });
            }
        }
        self.score_groups(text, raw, &kanji_break)
    }

    fn distribute_suffixes(
        &self,
        text: &[u16],
    ) -> Result<Vec<Vec<crate::support::SupportSuffixMatch>>> {
        let mut by_start = (0..text.len()).map(|_| Vec::new()).collect::<Vec<_>>();
        for end in 1..=text.len() {
            for matched in self
                .support
                .suffix_matches_ending_at(text, end, MAX_WORD_CODE_UNITS)?
            {
                let first_start = end.saturating_sub(MAX_WORD_CODE_UNITS);
                for start in first_start..matched.start {
                    let Some(values) = by_start.get_mut(start) else {
                        return Err(internal("suffix distribution start lies outside input"));
                    };
                    values.push(matched.clone());
                }
            }
        }
        Ok(by_start)
    }

    fn score_groups(
        &mut self,
        text: &[u16],
        raw: Vec<RawGroup>,
        kanji_break: &HashSet<usize>,
    ) -> Result<(Vec<SegmentGroup>, HashMap<i64, EngineCandidate>)> {
        let mut candidates = HashMap::new();
        let mut groups = Vec::new();
        let mut candidate_id = 1_i64;
        let mut group_id = 1_i64;
        for group in raw {
            let breaks = [group.start, group.end]
                .into_iter()
                .filter(|value| kanji_break.contains(value))
                .map(|value| value - group.start)
                .collect::<Vec<_>>();
            let mut segments = Vec::new();
            let mut matches = group.candidates.len();
            for candidate in group.candidates {
                let score_facts = if breaks.is_empty() {
                    candidate.score_facts.clone()
                } else {
                    self.lexicon.with_suru_break(&candidate.score_facts)?
                };
                let scored = score_candidate(
                    &score_facts,
                    &ScoreOptions {
                        final_word: group.end == text.len()
                            || (text.last() == Some(&0x30fc)
                                && group.end.checked_add(1) == Some(text.len())),
                        kanji_break: Some(breaks.clone()),
                        ..ScoreOptions::default()
                    },
                );
                let current_id = candidate_id;
                candidate_id = candidate_id
                    .checked_add(1)
                    .ok_or_else(|| internal("candidate ID overflow"))?;
                candidates.insert(
                    current_id,
                    EngineCandidate {
                        candidate: candidate.clone(),
                        score: f64::from(scored.score),
                        info: scored.info.clone(),
                    },
                );
                segments.push(segment(
                    current_id,
                    group.start,
                    group.end,
                    f64::from(scored.score),
                    &candidate,
                    scored.info,
                    word_kind(candidate.kind),
                ));

                if let Some(SegmentSplit {
                    candidate: split_candidate,
                    added_score,
                }) = self.lexicon.segment_split(&candidate)?
                {
                    let split_score = scored.score.checked_add(added_score).ok_or_else(|| {
                        internal("segment-split score overflowed signed 32-bit range")
                    })?;
                    let split_id = candidate_id;
                    candidate_id = candidate_id
                        .checked_add(1)
                        .ok_or_else(|| internal("candidate ID overflow"))?;
                    let split_info =
                        score_candidate(&split_candidate.score_facts, &ScoreOptions::default())
                            .info;
                    candidates.insert(
                        split_id,
                        EngineCandidate {
                            candidate: split_candidate.clone(),
                            score: f64::from(split_score),
                            info: split_info.clone(),
                        },
                    );
                    segments.push(segment(
                        split_id,
                        group.start,
                        group.end,
                        f64::from(split_score),
                        &split_candidate,
                        split_info,
                        RuleWordKind::Compound,
                    ));
                    matches = matches
                        .checked_add(1)
                        .ok_or_else(|| internal("segment match count overflow"))?;
                }
            }
            let segments = filter_and_cull_segments(&segments);
            if !segments.is_empty() {
                groups.push(SegmentGroup {
                    group_id,
                    start: group.start,
                    end: group.end,
                    segments,
                    matches,
                });
                group_id = group_id
                    .checked_add(1)
                    .ok_or_else(|| internal("group ID overflow"))?;
            }
        }
        Ok((groups, candidates))
    }
}

fn group_ends(char_class: CharClass, text: &[u16]) -> HashMap<usize, usize> {
    consecutive_char_groups(char_class, text, 0, text.len())
        .into_iter()
        .collect()
}

pub(super) fn sticky_positions(input: &[u16]) -> HashSet<usize> {
    let modifiers = MODIFIER_CHARACTERS
        .iter()
        .map(|(name, _)| *name)
        .chain(ITERATION_CHARACTERS.iter().map(|(name, _)| *name))
        .collect::<HashSet<_>>();
    let kana = KANA_CHARACTERS
        .iter()
        .map(|(name, _)| *name)
        .collect::<HashSet<_>>();
    let mut result = HashSet::new();
    for offset in 0..input.len() {
        let Some(char_class) = crate::characters::character_class(input[offset]) else {
            continue;
        };
        if char_class == "sokuon" && offset + 1 < input.len() {
            if crate::characters::character_class(input[offset + 1])
                .is_some_and(|next| kana.contains(next))
            {
                result.insert(offset + 1);
            }
        } else if modifiers.contains(char_class) {
            let permitted_end = offset + 1 == input.len()
                && (char_class == "longVowel"
                    || (offset > 0
                        && long_vowel_modifier_p(char_class, &input[offset - 1..offset])));
            if !permitted_end {
                result.insert(offset);
            }
        }
    }
    result
}

pub(super) fn record_kanji_breaks(
    surface: &[u16],
    start: usize,
    end: usize,
    breaks: &mut HashSet<usize>,
) {
    if surface == FORCE_KANJI_BREAK {
        breaks.extend(start + 1..end);
    } else if surface != NO_KANJI_BREAK {
        breaks.extend(sequential_kanji_positions(surface, start));
    }
}

fn segment(
    candidate_id: i64,
    start: usize,
    end: usize,
    score: f64,
    candidate: &MaterializedCandidate,
    score_info: crate::analyzer_model::ScoreInfo,
    rule_kind: RuleWordKind,
) -> Segment {
    let common = score_info.common;
    let (compound_end_seq, compound_end_text) = if candidate.kind == CandidateKind::Compound {
        candidate
            .components
            .last()
            .map_or((None, None), |component| {
                (
                    component.public_seq.map(i64::from),
                    Some(component.text.clone()),
                )
            })
    } else {
        (None, None)
    };
    Segment {
        candidate_id,
        start,
        end,
        score,
        common,
        entity: false,
        rules: Some(Arc::new(SegmentRuleFacts {
            text: candidate.text.clone(),
            word_kind: rule_kind,
            score_info: Some(score_info),
            compound_end_seq,
            compound_end_text,
        })),
    }
}

fn word_kind(kind: CandidateKind) -> RuleWordKind {
    match kind {
        CandidateKind::Simple => RuleWordKind::Simple,
        CandidateKind::Proxy => RuleWordKind::Proxy,
        CandidateKind::Compound => RuleWordKind::Compound,
        CandidateKind::Counter => RuleWordKind::Counter,
    }
}
