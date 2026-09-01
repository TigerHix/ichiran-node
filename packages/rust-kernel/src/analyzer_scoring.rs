use std::cmp::Ordering;

use crate::analyzer_model::{
    Conjugation, SCORE_FLAG_COMMON, SCORE_FLAG_LONG, SCORE_FLAG_PRIMARY, SCORE_FLAG_STRONG,
    ScoreBreakdown, ScoreCandidate, ScoreInfo, ScoreModifier, ScoreOptions, ScoreResult,
    ScoreSplit, ScoreWordKind, Segment, SplitScoreInfo, WordScoreFacts,
};
use crate::morphology::Route;

pub const SCORE_CUTOFF: i32 = 5;

const SKIP_WORDS: &[i64] = &[
    2_822_120, 2_013_800, 2_108_590, 2_029_040, 2_428_180, 2_654_250, 2_561_100, 2_210_270,
    2_210_710, 2_257_550, 2_210_320, 2_017_560, 2_394_890, 2_194_000, 2_568_000, 2_537_250,
    2_760_890, 2_831_062, 2_831_063, 2_029_030, 2_568_020, 900_000, 2_827_357,
];
const FINAL_PARTICLES: &[i64] = &[
    2_017_770, 2_425_930, 2_130_430, 2_029_130, 2_834_812, 2_718_360, 2_201_380, 2_722_170,
    2_751_630,
];
const SEMI_FINAL_PARTICLES: &[i64] = &[
    2_017_770, 2_425_930, 2_130_430, 2_029_130, 2_834_812, 2_718_360, 2_201_380, 2_722_170,
    2_751_630, 2_029_120, 2_086_640, 2_029_110, 2_029_080, 2_029_100,
];
const NON_FINAL_PARTICLES: &[i64] = &[2_139_720];
const COPULAE: &[i64] = &[2_089_020];
const NO_KANJI_BREAK_PENALTY: &[i64] = &[
    1_169_870, 1_198_360, 1_277_450, 2_028_980, 1_423_000, 1_164_690, 1_587_040, 2_827_864,
];

const STRONG_LENGTH: &[i32] = &[0, 1, 8, 24, 40, 60];
const WEAK_LENGTH: &[i32] = &[0, 1, 4, 9, 16, 25, 36];
const TAIL_LENGTH: &[i32] = &[0, 4, 9, 16, 24];
const LONG_TAIL_LENGTH: &[i32] = &[0, 4, 12, 18, 24];

pub fn score_candidate(candidate: &ScoreCandidate, options: &ScoreOptions) -> ScoreResult {
    match candidate {
        ScoreCandidate::Word(word) => score_word_full(word, options),
        ScoreCandidate::Compound(compound) => {
            let base_options = ScoreOptions {
                use_length: Some(mora_length(&compound.text)),
                modifier: compound.modifier,
                ..ScoreOptions::default()
            };
            let mut base = score_candidate(&compound.base, &base_options);
            base.info.conjugations.clone_from(&compound.conjugations);
            if let Some(breaks) = options.kanji_break.as_deref()
                && !breaks.is_empty()
            {
                base.score = score_kanji_break(
                    candidate,
                    breaks,
                    base.score,
                    &base.info,
                    &compound.text,
                    &base_options,
                );
            }
            base
        }
    }
}

fn score_word_full(candidate: &WordScoreFacts, options: &ScoreOptions) -> ScoreResult {
    let final_word = options.final_word;
    let use_length = truthy_length(options.use_length);
    let counter = candidate.kind == ScoreWordKind::Counter;
    let kanji = candidate.route == Route::Kanji;
    let katakana = !kanji && contains_katakana_unique(&candidate.true_text);
    let n_kanji = count_kanji(&candidate.text) as i32;
    let length = mora_length(&candidate.text).max(1);
    let mut ord = candidate.ord;

    let root = counter
        || candidate
            .entry
            .is_some_and(|entry| !candidate.conjugation_only && entry.root);
    let secondary = !candidate.conjugations.is_empty()
        && candidate
            .conjugations
            .iter()
            .all(|conjugation| conjugation.via.is_some());
    let conjugations: Vec<&Conjugation> = if secondary {
        candidate.conjugations.iter().collect()
    } else {
        candidate
            .conjugations
            .iter()
            .filter(|conjugation| conjugation.via.is_none())
            .collect()
    };
    let conjugation_types: Vec<u8> = conjugations
        .iter()
        .map(|conjugation| conjugation.property.kind)
        .collect();
    let conjugation_types_matter = root
        || options.use_length.is_some()
        || !conjugations
            .iter()
            .all(|conjugation| is_weak_conjugation(conjugation));
    let mut seq_set = Vec::new();
    if let Some(seq) = candidate.seq {
        seq_set.push(seq);
        seq_set.extend(conjugations.iter().map(|conjugation| conjugation.from));
    }

    let use_self_facts = candidate.seq.is_some() && root && use_length.is_none();
    let sequence_facts = if use_self_facts {
        candidate.self_facts
    } else {
        candidate.lineage
    };
    let has_scoring_sequence = if use_self_facts {
        candidate.seq.is_some()
    } else {
        !seq_set.is_empty()
    };
    let archived = has_scoring_sequence && sequence_facts.all_archived;
    let prefer_kana = sequence_facts.prefer_kana;
    let positions = if counter {
        vec!["ctr".to_owned()]
    } else {
        candidate.positions.clone()
    };

    let mut common = if candidate.conjugation_only {
        None
    } else {
        candidate.common
    };
    let mut common_of = common;
    let mut is_common = common.is_some();
    let particle = has_position(&positions, "prt");
    let semi_final_particle = candidate
        .seq
        .is_some_and(|seq| SEMI_FINAL_PARTICLES.contains(&seq));
    let non_final_particle = candidate
        .seq
        .is_some_and(|seq| NON_FINAL_PARTICLES.contains(&seq));
    let pronoun = has_position(&positions, "pn");
    let copula_da = seq_set.iter().any(|seq| COPULAE.contains(seq));

    let short_kanji = kanji
        && !prefer_kana
        && ((root && conjugations.is_empty())
            || (use_length.is_some() && conjugation_types.contains(&13)));
    let ranked_common = common.is_some_and(|value| is_common && value > 0 && value < 10);
    let threshold = if short_kanji || ranked_common {
        2
    } else if (conjugation_types.contains(&3) || conjugation_types.contains(&9))
        && use_length.is_none()
    {
        4
    } else {
        3
    };
    let long = length > threshold;
    let no_common_bonus =
        particle || !conjugation_types_matter || (!long && positions.as_slice() == ["int"]);

    if seq_set.iter().any(|seq| SKIP_WORDS.contains(seq))
        || (!final_word
            && candidate
                .seq
                .is_some_and(|seq| FINAL_PARTICLES.contains(&seq)))
        || (!root
            && !conjugations.is_empty()
            && conjugations
                .iter()
                .all(|conjugation| is_skipped_conjugation(conjugation)))
    {
        return ScoreResult {
            score: 0,
            info: empty_score_info(),
        };
    }

    if !(conjugations.is_empty() || ord == 0 && is_common) {
        if !is_common && let Some(inherited) = candidate.inherited_common {
            common = Some(0);
            common_of = Some(inherited);
            is_common = true;
        }
        if let Some(inherited) = candidate.inherited_ord
            && inherited < ord
        {
            ord = inherited;
        }
    }

    let primary = !archived
        && (candidate.entry.is_none()
            || (prefer_kana
                && conjugation_types_matter
                && !kanji
                && candidate
                    .entry
                    .is_some_and(|entry| !entry.primary_nokanji || candidate.nokanji))
            || ((ord == 0 || copula_da)
                && (kanji || conjugation_types_matter)
                && ((kanji && !prefer_kana)
                    || (is_common && pronoun)
                    || candidate.entry.is_some_and(|entry| entry.n_kanji == 0)))
            || (prefer_kana && kanji && ord == 0 && !sequence_facts.prefer_kana_on_ordinal_zero));

    let mut score = 1_i32;
    if primary {
        score += if long {
            10
        } else if secondary && !kanji {
            2
        } else if is_common && conjugation_types_matter {
            5
        } else if prefer_kana
            || candidate.entry.is_none()
            || candidate.entry.is_some_and(|entry| entry.n_kanji == 0)
        {
            3
        } else {
            2
        };
    }

    if particle && (final_word || !semi_final_particle) {
        score += 2;
        if is_common {
            score += 2 + length;
        }
        if final_word && !non_final_particle {
            if primary {
                score += 5;
            } else if semi_final_particle {
                score += 2;
            }
        }
    }

    if is_common
        && !no_common_bonus
        && let Some(rank) = common
    {
        let mut bonus = if secondary && use_length.is_none() {
            if kanji && primary { 4 } else { 2 }
        } else if long || copula_da || (root && (kanji || (primary && length > 2))) {
            if rank == 0 {
                10
            } else if !primary {
                (15 - rank).max(10)
            } else {
                (20 - rank).max(10)
            }
        } else if kanji {
            8
        } else if primary {
            4
        } else if length > 2 || (rank > 0 && rank < 10) {
            3
        } else {
            2
        };
        if bonus >= 10 && conjugation_types.contains(&10) {
            bonus -= 4;
        }
        score += bonus;
    }

    if long {
        score = score.max(length);
    }
    if kanji {
        score = score.max(if archived { 3 } else { 5 });
        if long && (n_kanji > 1 || length > 4) {
            score += 2;
        }
    }
    if counter {
        score = score.max(5);
    }

    let mut property_score = score;
    score = property_score
        * (length_coefficient(
            length,
            if kanji || katakana {
                STRONG_LENGTH
            } else {
                WEAK_LENGTH
            },
        ) + if n_kanji > 1 { (n_kanji - 1) * 5 } else { 0 });

    let mut use_length_bonus = 0;
    if let Some(use_length) = use_length {
        let extra_morae = use_length - length;
        use_length_bonus = property_score
            * length_coefficient(
                extra_morae,
                if length > 3 && (kanji || katakana) {
                    LONG_TAIL_LENGTH
                } else {
                    TAIL_LENGTH
                },
            );
        use_length_bonus +=
            property_score * options.modifier.multiplier * extra_morae + options.modifier.constant;
        score += use_length_bonus;
    }

    let mut split_info = None;
    if !counter && let Some(split) = &candidate.split {
        match split {
            ScoreSplit::Add(value) => {
                score += value;
                split_info = Some(SplitScoreInfo::Add(*value));
            }
            ScoreSplit::Proportional(value) => {
                let adjusted = (property_score + value).max(1);
                score = ceil_ratio(score, adjusted, property_score);
                property_score = adjusted;
            }
            ScoreSplit::Parts {
                score: split_score,
                parts,
                truncated_last,
            } => {
                let mut part_scores = Vec::with_capacity(parts.len() + 1);
                part_scores.push(*split_score);
                let mut source_length = 0_usize;
                let mut source_morae = 0_i32;
                for (index, part) in parts.iter().enumerate() {
                    let last = index + 1 == parts.len();
                    let part_length = part.text().len();
                    source_length += part_length;
                    let part_morae = mora_length(part.text());
                    source_morae += part_morae;
                    let truncated;
                    let score_part = if last && source_length > candidate.text.len() {
                        if let Some(explicit) = truncated_last {
                            explicit.as_ref()
                        } else {
                            let keep = (part_length + candidate.text.len() - source_length).max(1);
                            truncated = part.with_text(part.text()[..keep].to_vec());
                            &truncated
                        }
                    } else {
                        part
                    };
                    let part_options = ScoreOptions {
                        final_word: final_word && last,
                        use_length: if last {
                            use_length.map(|total| part_morae + total - source_morae)
                        } else {
                            None
                        },
                        modifier: if last {
                            options.modifier
                        } else {
                            ScoreModifier::default()
                        },
                        kanji_break: None,
                    };
                    part_scores.push(score_candidate(score_part, &part_options).score);
                }
                score = part_scores.iter().sum();
                split_info = Some(SplitScoreInfo::Parts(part_scores));
            }
        }
    }

    let mut flags = 0;
    if kanji || katakana {
        flags |= SCORE_FLAG_STRONG;
    }
    if primary {
        flags |= SCORE_FLAG_PRIMARY;
    }
    if is_common {
        flags |= SCORE_FLAG_COMMON;
    }
    if long {
        flags |= SCORE_FLAG_LONG;
    }
    let info = ScoreInfo {
        positions,
        seq_set,
        conjugations: conjugations.into_iter().cloned().collect(),
        common: is_common.then_some(common_of).flatten(),
        breakdown: ScoreBreakdown {
            property_score,
            kanji_break: options.kanji_break.clone(),
            use_length_bonus,
            split: split_info,
        },
        flags,
    };
    if let Some(breaks) = options.kanji_break.as_deref()
        && !breaks.is_empty()
    {
        score = score_kanji_break(
            &ScoreCandidate::Word(candidate.clone()),
            breaks,
            score,
            &info,
            &candidate.text,
            options,
        );
    }
    ScoreResult { score, info }
}

fn score_kanji_break(
    candidate: &ScoreCandidate,
    breaks: &[usize],
    score: i32,
    info: &ScoreInfo,
    text: &[u16],
    options: &ScoreOptions,
) -> i32 {
    let beginning = breaks == [0];
    let end = breaks.len() == 1 && !beginning;
    if info
        .seq_set
        .iter()
        .any(|seq| NO_KANJI_BREAK_PENALTY.contains(seq))
        || (beginning && text.starts_with(&[0x3059]))
    {
        return score;
    }
    let has_suru_position =
        has_position(&info.positions, "vs-s") || has_position(&info.positions, "v5s");
    if has_suru_position && let Some(suru) = candidate.suru_break() {
        let offset = mora_length(text) - mora_length(&suru.suffix_text);
        let suffix = score_candidate(
            &suru.candidate,
            &ScoreOptions {
                use_length: truthy_length(options.use_length).map(|value| value - offset),
                modifier: options.modifier,
                ..ScoreOptions::default()
            },
        );
        return score.min(suffix.score + 50);
    }
    let mut bonus = 0;
    if beginning && has_position(&info.positions, "num") {
        bonus += 5;
    }
    if beginning && (has_position(&info.positions, "suf") || has_position(&info.positions, "n-suf"))
    {
        bonus += 10;
    }
    if end && has_position(&info.positions, "pref") {
        bonus += 12;
    }
    if score >= SCORE_CUTOFF {
        SCORE_CUTOFF.max((score + 1) / 2 + bonus)
    } else {
        score
    }
}

fn empty_score_info() -> ScoreInfo {
    ScoreInfo {
        positions: Vec::new(),
        seq_set: Vec::new(),
        conjugations: Vec::new(),
        common: None,
        breakdown: ScoreBreakdown {
            property_score: 0,
            kanji_break: None,
            use_length_bonus: 0,
            split: None,
        },
        flags: 0,
    }
}

fn truthy_length(value: Option<i32>) -> Option<i32> {
    value.filter(|length| *length != 0)
}

fn length_coefficient(length: i32, coefficients: &[i32]) -> i32 {
    if length > 0 && (length as usize) < coefficients.len() {
        coefficients[length as usize]
    } else {
        (length * coefficients[coefficients.len() - 1]).div_euclid(coefficients.len() as i32 - 1)
    }
}

fn ceil_ratio(score: i32, adjusted: i32, property: i32) -> i32 {
    let numerator = i64::from(score) * i64::from(adjusted);
    ((numerator + i64::from(property) - 1) / i64::from(property)) as i32
}

fn is_weak_conjugation(conjugation: &Conjugation) -> bool {
    matches!(conjugation.property.kind, 51..=54)
        || (conjugation.property.kind == 9 && conjugation.property.negative == Some(true))
}

fn is_skipped_conjugation(conjugation: &Conjugation) -> bool {
    (conjugation.property.kind == 10 && conjugation.property.negative == Some(true))
        || (conjugation.property.kind == 3
            && conjugation.property.negative == Some(true)
            && conjugation.property.formal == Some(true))
        || (conjugation.property.pos == "vs-s" && conjugation.property.kind == 5)
}

fn has_position(positions: &[String], wanted: &str) -> bool {
    positions.iter().any(|position| position == wanted)
}

fn mora_length(text: &[u16]) -> i32 {
    let mut length = 0;
    let mut index = 0;
    while index < text.len() {
        let unit = text[index];
        if !matches!(
            unit,
            0x3063
                | 0x30c3
                | 0x3041
                | 0x30a1
                | 0x3043
                | 0x30a3
                | 0x3045
                | 0x30a5
                | 0x3047
                | 0x30a7
                | 0x3049
                | 0x30a9
                | 0x3083
                | 0x30e3
                | 0x3085
                | 0x30e5
                | 0x3087
                | 0x30e7
                | 0x30fc
        ) {
            length += 1;
        }
        index += if (0xd800..=0xdbff).contains(&unit)
            && text
                .get(index + 1)
                .is_some_and(|next| (0xdc00..=0xdfff).contains(next))
        {
            2
        } else {
            1
        };
    }
    length
}

fn contains_katakana_unique(text: &[u16]) -> bool {
    text.iter()
        .any(|unit| (0x30a1..=0x30fa).contains(unit) || matches!(*unit, 0x30fd | 0x30fe))
}

fn count_kanji(text: &[u16]) -> usize {
    text.iter()
        .filter(|unit| {
            matches!(**unit, 0x3005 | 0x30f6 | 0x3006) || (0x4e00..=0x9faf).contains(*unit)
        })
        .count()
}

pub fn compare_common(left: Option<i32>, right: Option<i32>) -> bool {
    match right {
        None => left.is_some(),
        Some(0) => left.is_some_and(|value| value > 0),
        Some(right) => left.is_some_and(|left| left > 0 && left < right),
    }
}

pub fn cull_segments(segments: &[Segment]) -> Vec<Segment> {
    if segments.is_empty() {
        return Vec::new();
    }
    let mut sorted = segments.to_vec();
    sorted.sort_by(|left, right| {
        if compare_common(left.common, right.common) {
            Ordering::Less
        } else if compare_common(right.common, left.common) {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    });
    sorted.sort_by(|left, right| right.score.total_cmp(&left.score));
    let Some(best) = sorted.first().map(|segment| segment.score) else {
        return Vec::new();
    };
    sorted
        .into_iter()
        .filter(|segment| segment.score * 2.0 >= best)
        .collect()
}

pub fn filter_and_cull_segments(segments: &[Segment]) -> Vec<Segment> {
    cull_segments(
        &segments
            .iter()
            .filter(|segment| segment.score >= f64::from(SCORE_CUTOFF))
            .cloned()
            .collect::<Vec<_>>(),
    )
}

pub fn select_alternatives(segments: &[Segment]) -> Vec<Segment> {
    let Some(best) = segments.first().map(|segment| segment.score) else {
        return Vec::new();
    };
    segments
        .iter()
        .filter(|segment| segment.score * 3.0 >= best * 2.0)
        .cloned()
        .collect()
}

#[cfg(test)]
mod tests;
