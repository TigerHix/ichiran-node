use crate::morphology::{MorphologyProperty, Route};
use crate::text::{count_kanji, mora_length};

pub(crate) struct WordFacts<'a> {
    pub(crate) text: &'a [u16],
    pub(crate) route: Route,
    pub(crate) seq: u32,
    pub(crate) ord: u8,
    pub(crate) common: Option<u8>,
    pub(crate) nokanji: bool,
    pub(crate) root: bool,
    pub(crate) n_kanji: u8,
    pub(crate) primary_nokanji: bool,
    pub(crate) conjugations: &'a [MorphologyProperty],
    pub(crate) positions: &'a [String],
    pub(crate) archived: bool,
    pub(crate) prefer_kana: bool,
    pub(crate) prefer_kana_zero: bool,
    pub(crate) inherited_common: Option<u8>,
}

pub(crate) fn score_word(candidate: WordFacts<'_>) -> i32 {
    let kanji = candidate.route == Route::Kanji;
    let n_kanji = count_kanji(candidate.text);
    let length = mora_length(candidate.text).max(1);
    let types: Vec<u8> = candidate
        .conjugations
        .iter()
        .map(|value| value.kind)
        .collect();
    let conjugation_types_matter =
        candidate.root || !candidate.conjugations.iter().all(is_weak_conjugation);
    let mut common = candidate.common;
    let mut is_common = common.is_some();
    if !candidate.conjugations.is_empty()
        && !is_common
        && let Some(_inherited) = candidate.inherited_common
    {
        common = Some(0);
        is_common = true;
    }
    let particle = candidate.positions.iter().any(|value| value == "prt");
    let pronoun = candidate.positions.iter().any(|value| value == "pn");
    let long = length
        > if (kanji
            && !candidate.prefer_kana
            && candidate.root
            && candidate.conjugations.is_empty())
            || (is_common && common.is_some_and(|value| value > 0 && value < 10))
        {
            2
        } else if types.contains(&3) || types.contains(&9) {
            4
        } else {
            3
        };
    let no_common_bonus = particle
        || !conjugation_types_matter
        || (!long && candidate.positions.len() == 1 && candidate.positions[0] == "int");
    if skipped_sequence(candidate.seq)
        || (!candidate.root
            && !candidate.conjugations.is_empty()
            && candidate.conjugations.iter().all(is_skipped_conjugation))
    {
        return 0;
    }
    let primary = !candidate.archived
        && ((candidate.prefer_kana
            && conjugation_types_matter
            && !kanji
            && (!candidate.primary_nokanji || candidate.nokanji))
            || ((candidate.ord == 0)
                && (kanji || conjugation_types_matter)
                && ((kanji && !candidate.prefer_kana)
                    || (is_common && pronoun)
                    || candidate.n_kanji == 0))
            || (candidate.prefer_kana
                && kanji
                && candidate.ord == 0
                && !candidate.prefer_kana_zero));
    let mut property_score = 1_i32;
    if primary {
        property_score += if long {
            10
        } else if is_common && conjugation_types_matter {
            5
        } else if candidate.prefer_kana || candidate.n_kanji == 0 {
            3
        } else {
            2
        };
    }
    if is_common && !no_common_bonus {
        let rank = common.unwrap_or(0) as i32;
        let bonus = if long || (candidate.root && kanji) {
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
        property_score += bonus;
    }
    if long {
        property_score = property_score.max(length as i32);
    }
    if kanji {
        property_score = property_score.max(if candidate.archived { 3 } else { 5 });
        if long && (n_kanji > 1 || length > 4) {
            property_score += 2;
        }
    }
    let coefficient = length_coefficient(
        length,
        if kanji {
            &[0, 1, 8, 24, 40, 60]
        } else {
            &[0, 1, 4, 9, 16, 25, 36]
        },
    );
    property_score * (coefficient + (n_kanji.saturating_sub(1) * 5) as i32)
}

fn length_coefficient(length: usize, coefficients: &[i32]) -> i32 {
    if length > 0 && length < coefficients.len() {
        coefficients[length]
    } else {
        (length as i32 * coefficients[coefficients.len() - 1]) / (coefficients.len() as i32 - 1)
    }
}

fn is_weak_conjugation(property: &MorphologyProperty) -> bool {
    matches!(property.kind, 51..=54) || (property.kind == 9 && property.negative == Some(true))
}

fn is_skipped_conjugation(property: &MorphologyProperty) -> bool {
    (property.kind == 10 && property.negative == Some(true))
        || (property.kind == 3 && property.negative == Some(true) && property.formal == Some(true))
        || (property.pos == "vs-s" && property.kind == 5)
}

fn skipped_sequence(seq: u32) -> bool {
    const VALUES: [u32; 23] = [
        2_822_120, 2_013_800, 2_108_590, 2_029_040, 2_428_180, 2_654_250, 2_561_100, 2_210_270,
        2_210_710, 2_257_550, 2_210_320, 2_017_560, 2_394_890, 2_194_000, 2_568_000, 2_537_250,
        2_760_890, 2_831_062, 2_831_063, 2_029_030, 2_568_020, 900_000, 2_827_357,
    ];
    VALUES.contains(&seq)
}
