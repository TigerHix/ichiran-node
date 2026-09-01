use crate::analyzer_model::{
    PathAdjustment, PathTransition, RuleWordKind, SCORE_FLAG_COMMON, SCORE_FLAG_LONG,
    SCORE_FLAG_PRIMARY, SCORE_FLAG_STRONG, Segment, SegmentGroup,
};

type SegmentFilter = fn(&Segment) -> bool;

const NOUN_PARTICLES: &[i64] = &[
    2_028_920, 2_028_930, 2_028_990, 2_028_980, 2_029_000, 1_007_340, 1_579_080, 1_525_680,
    2_028_940, 1_582_300, 2_215_430, 1_469_800, 1_009_990, 2_029_010, 1_005_120, 2_034_520,
    1_005_120, 1_008_490, 1_008_530, 1_008_590, 2_028_950, 2_028_960, 1_009_600,
];
const SEMI_FINAL_PARTICLES: &[i64] = &[
    2_017_770, 2_425_930, 2_130_430, 2_029_130, 2_834_812, 2_718_360, 2_201_380, 2_722_170,
    2_751_630, 2_029_120, 2_086_640, 2_029_110, 2_029_080, 2_029_100,
];

#[derive(Clone, Copy)]
struct Segfilter {
    left: SegmentFilter,
    right: SegmentFilter,
    allow_first: bool,
}

impl Segfilter {
    fn apply(
        self,
        left: Option<SegmentGroup>,
        right: SegmentGroup,
    ) -> Vec<(Option<SegmentGroup>, SegmentGroup)> {
        let (right_yes, right_no) = classify(&right.segments, self.right);
        if right_yes.is_empty() || (self.allow_first && left.is_none()) {
            return vec![(left, right)];
        }
        let Some(left) = left else {
            return group_with(right, right_no)
                .map(|right| (None, right))
                .into_iter()
                .collect();
        };
        if left.end != right.start {
            return group_with(right, right_no)
                .map(|right| (Some(left), right))
                .into_iter()
                .collect();
        }
        let (left_yes, left_no) = classify(&left.segments, self.left);
        if left_no.is_empty() {
            return vec![(Some(left), right)];
        }
        let mut output = Vec::with_capacity(2);
        if let Some(filtered_right) = group_with(right.clone(), right_no) {
            output.push((Some(left.clone()), filtered_right));
        }
        if let (Some(filtered_left), Some(filtered_right)) =
            (group_with(left, left_yes), group_with(right, right_yes))
        {
            output.push((Some(filtered_left), filtered_right));
        }
        output
    }

    fn non_adjacent_right(self, right: SegmentGroup) -> Option<SegmentGroup> {
        let (right_yes, right_no) = classify(&right.segments, self.right);
        if right_yes.is_empty() {
            Some(right)
        } else {
            group_with(right, right_no)
        }
    }
}

const SEGFILTERS: &[Segfilter] = &[
    Segfilter {
        left: |segment| has_conjugation_type(segment, 13),
        right: |segment| has_sequence(segment, &[1_342_560]),
        allow_first: false,
    },
    Segfilter {
        left: |segment| !has_sequence(segment, &[2_221_640]),
        right: |segment| has_sequence(segment, &[1_577_980]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !is_simple_with_sequence(segment, NOUN_PARTICLES),
        right: |segment| has_sequence(segment, &[2_139_720, 2_849_370, 2_849_387]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| has_sequence(segment, &[2_029_010]),
        right: |segment| has_sequence(segment, &[2_087_020]),
        allow_first: false,
    },
    Segfilter {
        left: |_| false,
        right: |segment| compound_ends_in_text(segment, &["ちゃい", "いか", "とか", "とき", "い"]),
        allow_first: false,
    },
    Segfilter {
        left: |_| false,
        right: |segment| {
            has_conjugation_type(segment, 54) && text_ends_with(segment_text(segment), "好き")
        },
        allow_first: false,
    },
    Segfilter {
        left: |segment| !compound_ends_in_text(segment, &["いろ"]),
        right: |segment| text_starts_with(segment_text(segment), "く"),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !compound_ends_in_sequence(segment, &[2_029_120]),
        right: |segment| text_starts_with(segment_text(segment), "え"),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !compound_ends_in_sequence(segment, &[2_028_920]),
        right: |segment| has_sequence(segment, &[1_529_520, 1_296_400, 2_139_720]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !has_sequence(segment, &[1_469_800]),
        right: |segment| has_sequence(segment, &[1_601_080]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !has_sequence(segment, &[2_837_117]),
        right: |segment| has_sequence(segment, &[1_589_350, 1_587_040]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !has_sequence(segment, &[1_008_490]),
        right: |segment| has_sequence(segment, &[2_086_960]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !has_sequence(segment, &[2_089_020]) || has_sequence(segment, &[2_028_980]),
        right: |segment| has_sequence(segment, &[1_157_170, 2_424_740, 1_305_070]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !has_sequence(segment, &[1_896_380, 2_422_860]),
        right: |segment| has_sequence(segment, &[2_830_009, 1_547_720]),
        allow_first: true,
    },
    Segfilter {
        left: |segment| !is_simple_with_sequence(segment, NOUN_PARTICLES),
        right: |segment| has_sequence(segment, &[1_247_260]),
        allow_first: false,
    },
    Segfilter {
        left: |segment| !has_sequence(segment, &[2_028_940]),
        right: |segment| has_sequence(segment, &[1_009_980]),
        allow_first: true,
    },
];

fn classify(segments: &[Segment], filter: SegmentFilter) -> (Vec<Segment>, Vec<Segment>) {
    let mut yes = Vec::new();
    let mut no = Vec::new();
    for segment in segments {
        if filter(segment) {
            yes.push(segment.clone());
        } else {
            no.push(segment.clone());
        }
    }
    (yes, no)
}

fn group_with(mut group: SegmentGroup, segments: Vec<Segment>) -> Option<SegmentGroup> {
    if segments.is_empty() {
        None
    } else {
        group.segments = segments;
        Some(group)
    }
}

fn has_sequence(segment: &Segment, values: &[i64]) -> bool {
    segment
        .rules
        .as_ref()
        .and_then(|rules| rules.score_info.as_ref())
        .is_some_and(|info| values.iter().any(|value| info.seq_set.contains(value)))
}

fn is_simple_with_sequence(segment: &Segment, values: &[i64]) -> bool {
    segment
        .rules
        .as_ref()
        .is_some_and(|rules| rules.word_kind == RuleWordKind::Simple)
        && has_sequence(segment, values)
}

fn has_conjugation_type(segment: &Segment, kind: u8) -> bool {
    segment
        .rules
        .as_ref()
        .and_then(|rules| rules.score_info.as_ref())
        .is_some_and(|info| {
            info.conjugations
                .iter()
                .any(|conjugation| conjugation.property.kind == kind)
        })
}

fn compound_ends_in_sequence(segment: &Segment, values: &[i64]) -> bool {
    segment
        .rules
        .as_ref()
        .and_then(|rules| rules.compound_end_seq)
        .is_some_and(|seq| values.contains(&seq))
}

fn compound_ends_in_text(segment: &Segment, values: &[&str]) -> bool {
    segment
        .rules
        .as_ref()
        .and_then(|rules| rules.compound_end_text.as_deref())
        .is_some_and(|text| values.iter().any(|value| text_equals(text, value)))
}

fn segment_text(segment: &Segment) -> &[u16] {
    segment
        .rules
        .as_ref()
        .map_or(&[], |rules| rules.text.as_slice())
}

fn text_starts_with(text: &[u16], prefix: &str) -> bool {
    let mut text = text.iter().copied();
    prefix.encode_utf16().all(|unit| text.next() == Some(unit))
}

fn text_ends_with(text: &[u16], suffix: &str) -> bool {
    let suffix_length = suffix.encode_utf16().count();
    text.get(text.len().saturating_sub(suffix_length)..)
        .is_some_and(|text| text_equals(text, suffix))
}

fn text_equals(text: &[u16], value: &str) -> bool {
    text.iter().copied().eq(value.encode_utf16())
}

pub fn apply_segfilters(
    left: Option<SegmentGroup>,
    right: SegmentGroup,
) -> Vec<(Option<SegmentGroup>, SegmentGroup)> {
    let mut splits = vec![(left, right)];
    for filter in SEGFILTERS {
        let mut next = Vec::new();
        for (left, right) in splits {
            next.extend(filter.apply(left, right));
        }
        splits = next;
    }
    splits
}

fn non_adjacent_right(mut group: SegmentGroup) -> Option<SegmentGroup> {
    for filter in SEGFILTERS {
        group = filter.non_adjacent_right(group)?;
    }
    Some(group)
}

fn flag(segment: &Segment, value: u8) -> bool {
    segment
        .rules
        .as_ref()
        .and_then(|rules| rules.score_info.as_ref())
        .is_some_and(|info| info.flags & value != 0)
}

fn has_position(segment: &Segment, positions: &[&str]) -> bool {
    segment
        .rules
        .as_ref()
        .and_then(|rules| rules.score_info.as_ref())
        .is_some_and(|info| {
            positions
                .iter()
                .any(|position| info.positions.iter().any(|actual| actual == position))
        })
}

fn is_noun(segment: &Segment) -> bool {
    let noun_shape = flag(segment, SCORE_FLAG_LONG)
        || flag(segment, SCORE_FLAG_STRONG)
        || (flag(segment, SCORE_FLAG_PRIMARY) && flag(segment, SCORE_FLAG_COMMON));
    if noun_shape && has_position(segment, &["n", "n-adv", "n-t", "adj-na", "n-suf", "pn"]) {
        return true;
    }
    segment.rules.as_ref().is_some_and(|rules| {
        rules.word_kind == RuleWordKind::Counter
            && rules
                .score_info
                .as_ref()
                .is_some_and(|info| !info.seq_set.is_empty())
    })
}

fn is_position(
    segment: &Segment,
    positions: &[&str],
    shape: fn(bool, bool, bool, bool) -> bool,
) -> bool {
    shape(
        flag(segment, SCORE_FLAG_STRONG),
        flag(segment, SCORE_FLAG_PRIMARY),
        flag(segment, SCORE_FLAG_COMMON),
        flag(segment, SCORE_FLAG_LONG),
    ) && has_position(segment, positions)
}

#[derive(Clone, Copy)]
enum SynergyScore {
    Constant(i32),
    Span(fn(&SegmentGroup, &SegmentGroup) -> i32),
}

#[derive(Clone, Copy)]
struct SynergyRule {
    left: SegmentFilter,
    right: SegmentFilter,
    description: &'static str,
    connector: &'static str,
    score: SynergyScore,
}

const SYNERGIES: &[SynergyRule] = &[
    SynergyRule {
        left: is_noun,
        right: |segment| has_sequence(segment, NOUN_PARTICLES),
        description: "noun+prt",
        connector: " ",
        score: SynergyScore::Span(|_, right| 10 + 4 * (right.end - right.start) as i32),
    },
    SynergyRule {
        left: is_noun,
        right: |segment| has_sequence(segment, &[2_089_020]),
        description: "noun+da",
        connector: " ",
        score: SynergyScore::Constant(10),
    },
    SynergyRule {
        left: |segment| has_sequence(segment, &[1_469_800, 2_139_720]),
        right: |segment| has_sequence(segment, &[2_089_020, 1_007_370, 1_928_670]),
        description: "no da/desu",
        connector: " ",
        score: SynergyScore::Constant(15),
    },
    SynergyRule {
        left: |segment| has_sequence(segment, &[2_137_720]),
        right: |segment| has_sequence(segment, &[2_140_410]),
        description: "sou na n da",
        connector: " ",
        score: SynergyScore::Constant(50),
    },
    SynergyRule {
        left: |segment| {
            is_position(segment, &["adj-no"], |strong, primary, common, long| {
                strong || long || (primary && common)
            })
        },
        right: |segment| has_sequence(segment, &[1_469_800]),
        description: "no-adjective",
        connector: " ",
        score: SynergyScore::Constant(15),
    },
    SynergyRule {
        left: |segment| {
            is_position(segment, &["adj-na"], |strong, primary, common, long| {
                strong || long || (primary && common)
            })
        },
        right: |segment| has_sequence(segment, &[2_029_110, 2_028_990]),
        description: "na-adjective",
        connector: " ",
        score: SynergyScore::Constant(15),
    },
    SynergyRule {
        left: |segment| {
            is_position(segment, &["adv-to"], |strong, primary, _, long| {
                strong || long || primary
            })
        },
        right: |segment| has_sequence(segment, &[1_008_490]),
        description: "to-adverb",
        connector: " ",
        score: SynergyScore::Span(|left, _| 10 + 10 * (left.end - left.start) as i32),
    },
    SynergyRule {
        left: is_noun,
        right: |segment| has_sequence(segment, &[1_620_400, 2_083_570]),
        description: "suffix-chu",
        connector: "-",
        score: SynergyScore::Constant(12),
    },
    SynergyRule {
        left: is_noun,
        right: |segment| has_sequence(segment, &[1_416_220]),
        description: "suffix-tachi",
        connector: "-",
        score: SynergyScore::Constant(10),
    },
    SynergyRule {
        left: is_noun,
        right: |segment| has_sequence(segment, &[1_361_140]),
        description: "suffix-buri",
        connector: "",
        score: SynergyScore::Constant(40),
    },
    SynergyRule {
        left: is_noun,
        right: |segment| has_sequence(segment, &[1_375_260]),
        description: "suffix-sei",
        connector: "",
        score: SynergyScore::Constant(12),
    },
    SynergyRule {
        left: |segment| has_sequence(segment, &[1_270_190]),
        right: |segment| is_position(segment, &["n"], |strong, _, _, long| strong || long),
        description: "o+noun",
        connector: "",
        score: SynergyScore::Constant(10),
    },
    SynergyRule {
        left: |segment| has_sequence(segment, &[2_242_840, 1_922_780, 2_423_740]),
        right: |segment| is_position(segment, &["n"], |strong, _, _, _| strong),
        description: "kanji prefix+noun",
        connector: "",
        score: SynergyScore::Constant(15),
    },
    SynergyRule {
        left: |segment| compound_ends_in_sequence(segment, &[2_028_920]),
        right: |segment| {
            has_sequence(
                segment,
                &[1_000_730, 1_612_750, 1_409_110, 2_829_697, 1_587_610],
            )
        },
        description: "shicha ikenai",
        connector: " ",
        score: SynergyScore::Constant(50),
    },
    SynergyRule {
        left: |segment| has_sequence(segment, &[1_005_460]),
        right: |segment| {
            segment
                .rules
                .as_ref()
                .and_then(|rules| rules.score_info.as_ref())
                .is_some_and(|info| {
                    info.conjugations
                        .iter()
                        .any(|conjugation| conjugation.property.negative != Some(false))
                })
        },
        description: "shika+neg",
        connector: " ",
        score: SynergyScore::Constant(50),
    },
    SynergyRule {
        left: |segment| has_sequence(segment, &[1_469_800]),
        right: |segment| has_sequence(segment, &[1_432_920]),
        description: "no toori",
        connector: " ",
        score: SynergyScore::Constant(50),
    },
    SynergyRule {
        left: |segment| is_position(segment, &["ctr"], |_, _, _, _| true),
        right: |segment| has_sequence(segment, &[2_854_117, 2_084_550]),
        description: "",
        connector: "",
        score: SynergyScore::Constant(20),
    },
];

fn adjustment(
    left: &SegmentGroup,
    right: &SegmentGroup,
    description: &str,
    connector: &str,
    score: i32,
) -> PathAdjustment {
    PathAdjustment {
        start: left.end,
        end: right.start,
        description: description.to_owned(),
        connector: connector.to_owned(),
        score: f64::from(score),
    }
}

fn short_penalty_group(group: &SegmentGroup, except_to: bool) -> bool {
    group.segments.first().is_some_and(|first| {
        group.end - group.start <= 1
            && !flag(first, SCORE_FLAG_STRONG)
            && (!except_to || !text_equals(segment_text(first), "と"))
    })
}

pub fn short_penalty_left(group: &SegmentGroup) -> bool {
    short_penalty_group(group, false)
}

fn penalty(left: &SegmentGroup, right: &SegmentGroup) -> Option<PathAdjustment> {
    if left.end == right.start
        && left
            .segments
            .iter()
            .any(|segment| has_sequence(segment, SEMI_FINAL_PARTICLES))
    {
        return Some(adjustment(left, right, "semi-final not final", " ", -15));
    }
    (short_penalty_left(left) && short_penalty_group(right, true))
        .then(|| adjustment(left, right, "short", " ", -9))
}

fn synergy_transitions(left: &SegmentGroup, right: &SegmentGroup) -> Vec<PathTransition> {
    if left.end != right.start {
        return Vec::new();
    }
    let mut output = Vec::new();
    for rule in SYNERGIES {
        let left_segments: Vec<_> = left
            .segments
            .iter()
            .filter(|segment| (rule.left)(segment))
            .cloned()
            .collect();
        let right_segments: Vec<_> = right
            .segments
            .iter()
            .filter(|segment| (rule.right)(segment))
            .cloned()
            .collect();
        if left_segments.is_empty() || right_segments.is_empty() {
            continue;
        }
        let score = match rule.score {
            SynergyScore::Constant(score) => score,
            SynergyScore::Span(score) => score(left, right),
        };
        let mut filtered_left = left.clone();
        filtered_left.segments = left_segments;
        let mut filtered_right = right.clone();
        filtered_right.segments = right_segments;
        output.push(PathTransition {
            right: filtered_right,
            adjustment: Some(adjustment(
                left,
                right,
                rule.description,
                rule.connector,
                score,
            )),
            left: filtered_left,
        });
    }
    output
}

pub fn resolve_initial_rules(group: &SegmentGroup) -> Vec<SegmentGroup> {
    apply_segfilters(None, group.clone())
        .into_iter()
        .map(|(_, right)| right)
        .collect()
}

pub fn resolve_rule_transitions(left: &SegmentGroup, right: &SegmentGroup) -> Vec<PathTransition> {
    if left.end != right.start {
        return non_adjacent_right(right.clone())
            .map(|right| PathTransition {
                adjustment: penalty(left, &right),
                right,
                left: left.clone(),
            })
            .into_iter()
            .collect();
    }
    let mut output = Vec::new();
    for (left, right) in apply_segfilters(Some(left.clone()), right.clone()) {
        let Some(left) = left else {
            continue;
        };
        output.push(PathTransition {
            adjustment: penalty(&left, &right),
            right: right.clone(),
            left: left.clone(),
        });
        output.extend(synergy_transitions(&left, &right));
    }
    output
}

#[cfg(test)]
mod tests;
