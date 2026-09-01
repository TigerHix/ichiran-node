use super::*;
use crate::analyzer_model::{
    Conjugation, ConjugationProperty, ScoreBreakdown, ScoreInfo, SegmentRuleFacts, SplitScoreInfo,
    utf16,
};

#[derive(Clone)]
struct RuleSpec {
    id: i64,
    text: &'static str,
    word_kind: RuleWordKind,
    seq_set: Vec<i64>,
    positions: Vec<String>,
    flags: u8,
    conjugations: Vec<Conjugation>,
    compound_end_seq: Option<i64>,
    compound_end_text: Option<&'static str>,
}

impl RuleSpec {
    fn new(id: i64, text: &'static str) -> Self {
        Self {
            id,
            text,
            word_kind: RuleWordKind::Simple,
            seq_set: Vec::new(),
            positions: Vec::new(),
            flags: SCORE_FLAG_STRONG,
            conjugations: Vec::new(),
            compound_end_seq: None,
            compound_end_text: None,
        }
    }

    fn seq(mut self, seq: i64) -> Self {
        self.seq_set = vec![seq];
        self
    }

    fn position(mut self, position: &str) -> Self {
        self.positions = vec![position.to_owned()];
        self
    }

    fn flags(mut self, flags: u8) -> Self {
        self.flags = flags;
        self
    }

    fn kind(mut self, kind: RuleWordKind) -> Self {
        self.word_kind = kind;
        self
    }

    fn conjugation(mut self, kind: u8, negative: Option<bool>) -> Self {
        self.conjugations = vec![Conjugation {
            seq: 9_000_001 + i64::from(kind),
            from: 8_000_001,
            via: None,
            property: ConjugationProperty {
                pos: "v1".to_owned(),
                kind,
                negative,
                formal: Some(false),
            },
        }];
        self
    }

    fn compound_end(mut self, seq: Option<i64>, text: &'static str) -> Self {
        self.word_kind = RuleWordKind::Compound;
        self.compound_end_seq = seq;
        self.compound_end_text = Some(text);
        self
    }
}

fn noun(id: i64, text: &'static str) -> RuleSpec {
    RuleSpec::new(id, text)
        .position("n")
        .flags(SCORE_FLAG_STRONG | SCORE_FLAG_PRIMARY)
}

fn segment(spec: &RuleSpec, start: usize, end: usize) -> Segment {
    Segment {
        candidate_id: spec.id,
        start,
        end,
        score: 20.0,
        common: None,
        entity: false,
        rules: Some(SegmentRuleFacts {
            text: utf16(spec.text),
            word_kind: spec.word_kind,
            score_info: Some(ScoreInfo {
                positions: spec.positions.clone(),
                seq_set: spec.seq_set.clone(),
                conjugations: spec.conjugations.clone(),
                common: None,
                breakdown: ScoreBreakdown {
                    property_score: 1,
                    kanji_break: None,
                    use_length_bonus: 0,
                    split: None::<SplitScoreInfo>,
                },
                flags: spec.flags,
            }),
            compound_end_seq: spec.compound_end_seq,
            compound_end_text: spec.compound_end_text.map(utf16),
        }),
    }
}

fn group(group_id: i64, start: usize, end: usize, specs: &[RuleSpec]) -> SegmentGroup {
    SegmentGroup {
        group_id,
        start,
        end,
        segments: specs.iter().map(|spec| segment(spec, start, end)).collect(),
        matches: specs.len(),
    }
}

fn ids(group: &SegmentGroup) -> Vec<i64> {
    group
        .segments
        .iter()
        .map(|segment| segment.candidate_id)
        .collect()
}

#[test]
fn every_registered_synergy_matches_typescript_fixture() {
    let cases = vec![
        (
            "noun+prt",
            18,
            noun(1, "名"),
            RuleSpec::new(2, "は").seq(2_028_920),
        ),
        (
            "noun+da",
            10,
            noun(3, "名"),
            RuleSpec::new(4, "だ").seq(2_089_020),
        ),
        (
            "no da/desu",
            15,
            RuleSpec::new(5, "の").seq(1_469_800),
            RuleSpec::new(6, "です").seq(1_007_370),
        ),
        (
            "sou na n da",
            50,
            RuleSpec::new(7, "そう").seq(2_137_720),
            RuleSpec::new(8, "なんだ").seq(2_140_410),
        ),
        (
            "no-adjective",
            15,
            RuleSpec::new(9, "特別")
                .position("adj-no")
                .flags(SCORE_FLAG_STRONG | SCORE_FLAG_PRIMARY),
            RuleSpec::new(10, "の").seq(1_469_800),
        ),
        (
            "na-adjective",
            15,
            RuleSpec::new(11, "静か")
                .position("adj-na")
                .flags(SCORE_FLAG_STRONG | SCORE_FLAG_PRIMARY),
            RuleSpec::new(12, "な").seq(2_029_110),
        ),
        (
            "to-adverb",
            30,
            RuleSpec::new(13, "堂々")
                .position("adv-to")
                .flags(SCORE_FLAG_PRIMARY),
            RuleSpec::new(14, "と").seq(1_008_490),
        ),
        (
            "suffix-chu",
            12,
            noun(15, "名"),
            RuleSpec::new(16, "中").seq(1_620_400),
        ),
        (
            "suffix-tachi",
            10,
            noun(17, "名"),
            RuleSpec::new(18, "達").seq(1_416_220),
        ),
        (
            "suffix-buri",
            40,
            noun(19, "名"),
            RuleSpec::new(20, "振り").seq(1_361_140),
        ),
        (
            "suffix-sei",
            12,
            noun(21, "名"),
            RuleSpec::new(22, "性").seq(1_375_260),
        ),
        (
            "o+noun",
            10,
            RuleSpec::new(23, "お").seq(1_270_190),
            noun(24, "名"),
        ),
        (
            "kanji prefix+noun",
            15,
            RuleSpec::new(25, "未").seq(2_242_840),
            noun(26, "名"),
        ),
        (
            "shicha ikenai",
            50,
            RuleSpec::new(27, "しちゃ").compound_end(Some(2_028_920), "は"),
            RuleSpec::new(28, "いけない").seq(1_000_730),
        ),
        (
            "shika+neg",
            50,
            RuleSpec::new(29, "しか").seq(1_005_460),
            RuleSpec::new(30, "ない").conjugation(1, None),
        ),
        (
            "no toori",
            50,
            RuleSpec::new(31, "の").seq(1_469_800),
            RuleSpec::new(32, "通り").seq(1_432_920),
        ),
        (
            "",
            20,
            RuleSpec::new(33, "三人")
                .kind(RuleWordKind::Counter)
                .position("ctr")
                .seq(1),
            RuleSpec::new(34, "置き").seq(2_854_117),
        ),
    ];
    for (description, score, left, right) in cases {
        let transitions =
            resolve_rule_transitions(&group(1, 0, 2, &[left]), &group(2, 2, 4, &[right]));
        let found = transitions
            .iter()
            .filter_map(|transition| transition.adjustment.as_ref())
            .find(|adjustment| adjustment.description == description && adjustment.score > 0.0);
        assert_eq!(
            found.map(|adjustment| adjustment.score),
            Some(f64::from(score)),
            "{description}"
        );
    }
}

#[test]
fn filters_penalty_priority_and_non_adjacent_behavior_match_typescript() {
    let neutral = RuleSpec::new(100, "普通").position("n");
    let adjacent = resolve_rule_transitions(
        &group(
            1,
            0,
            2,
            &[
                RuleSpec::new(101, "連用").conjugation(13, Some(false)),
                neutral.clone(),
            ],
        ),
        &group(
            2,
            2,
            4,
            &[
                RuleSpec::new(102, "始める").seq(1_342_560),
                RuleSpec::new(103, "普通"),
            ],
        ),
    );
    assert_eq!(
        adjacent
            .iter()
            .map(|transition| (ids(&transition.left), ids(&transition.right)))
            .collect::<Vec<_>>(),
        [(vec![101, 100], vec![103]), (vec![101], vec![102])]
    );

    let non_adjacent = resolve_rule_transitions(
        &group(1, 0, 2, std::slice::from_ref(&neutral)),
        &group(
            2,
            3,
            5,
            &[
                RuleSpec::new(104, "始める").seq(1_342_560),
                RuleSpec::new(105, "普通"),
            ],
        ),
    );
    assert_eq!(ids(&non_adjacent[0].right), [105]);

    let compound_filter = resolve_rule_transitions(
        &group(
            1,
            0,
            2,
            &[
                RuleSpec::new(106, "語尾").compound_end(None, "ちゃい"),
                neutral.clone(),
            ],
        ),
        &group(
            2,
            2,
            4,
            &[
                RuleSpec::new(107, "語尾").compound_end(None, "ちゃい"),
                RuleSpec::new(108, "普通"),
            ],
        ),
    );
    assert_eq!(compound_filter.len(), 1);
    assert_eq!(ids(&compound_filter[0].right), [108]);

    let mononi = apply_segfilters(
        Some(group(
            1,
            0,
            1,
            &[RuleSpec::new(120, "も").seq(2_028_940), neutral.clone()],
        )),
        group(
            2,
            1,
            3,
            &[
                RuleSpec::new(121, "のに").seq(1_009_980),
                RuleSpec::new(122, "普通"),
            ],
        ),
    );
    assert_eq!(
        mononi
            .iter()
            .map(|(left, right)| (left.as_ref().map_or_else(Vec::new, ids), ids(right)))
            .collect::<Vec<_>>(),
        [(vec![120, 100], vec![122]), (vec![100], vec![121])]
    );

    let penalty = resolve_rule_transitions(
        &group(
            1,
            0,
            1,
            &[RuleSpec::new(113, "かい").seq(2_017_770).flags(0)],
        ),
        &group(2, 1, 2, &[RuleSpec::new(114, "な").flags(0)]),
    );
    assert_eq!(
        penalty[0]
            .adjustment
            .as_ref()
            .map(|value| value.description.as_str()),
        Some("semi-final not final")
    );
}

fn hash_u64(mut hash: u64, value: i64) -> u64 {
    for byte in value.to_le_bytes() {
        hash ^= u64::from(byte);
        hash = hash.wrapping_mul(1_099_511_628_211);
    }
    hash
}

#[test]
fn deterministic_mixed_feature_corpus_matches_typescript_fixture() {
    let catalog = vec![
        RuleSpec::new(200, "普通").position("n"),
        RuleSpec::new(201, "始める").seq(1_342_560),
        RuleSpec::new(202, "連用").conjugation(13, Some(false)),
        RuleSpec::new(203, "いる").seq(1_577_980),
        RuleSpec::new(204, "つ").seq(2_221_640),
        RuleSpec::new(205, "ん").seq(2_139_720),
        RuleSpec::new(206, "は").seq(2_028_920),
        RuleSpec::new(207, "ちゃい").compound_end(None, "ちゃい"),
        RuleSpec::new(208, "大好き").conjugation(54, Some(false)),
        RuleSpec::new(209, "くる"),
        RuleSpec::new(210, "える"),
        RuleSpec::new(211, "さ").compound_end(Some(2_029_120), "さ"),
        RuleSpec::new(212, "静か")
            .position("adj-na")
            .flags(SCORE_FLAG_STRONG | SCORE_FLAG_PRIMARY),
        RuleSpec::new(213, "三人")
            .kind(RuleWordKind::Counter)
            .position("ctr")
            .seq(1),
        RuleSpec::new(214, "しか").seq(1_005_460),
        RuleSpec::new(215, "ない").conjugation(1, None),
        RuleSpec::new(216, "あ").flags(0),
        RuleSpec::new(217, "と").flags(0).seq(1_008_490),
        RuleSpec::new(218, "君").seq(1_247_260),
        RuleSpec::new(219, "だ").seq(2_089_020),
    ];
    let mut state = 0x1234_abcd_u32;
    let mut random = || {
        state = state.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
        state
    };
    let mut hash = 14_695_981_039_346_656_037_u64;
    for _ in 0..250 {
        let left_len = 1 + random() % 3;
        let left: Vec<_> = (0..left_len)
            .map(|_| catalog[random() as usize % catalog.len()].clone())
            .collect();
        let right_len = 1 + random() % 3;
        let right: Vec<_> = (0..right_len)
            .map(|_| catalog[random() as usize % catalog.len()].clone())
            .collect();
        let adjacent = random() % 3 != 0;
        let right_start = if adjacent { 2 } else { 3 };
        let transitions = resolve_rule_transitions(
            &group(1, 0, 2, &left),
            &group(2, right_start, right_start + 2, &right),
        );
        hash = hash_u64(hash, transitions.len() as i64);
        for transition in transitions {
            for id in ids(&transition.left) {
                hash = hash_u64(hash, id);
            }
            hash = hash_u64(hash, -1);
            hash = hash_u64(
                hash,
                transition
                    .adjustment
                    .as_ref()
                    .map_or(0.0, |value| value.score) as i64,
            );
            for id in ids(&transition.right) {
                hash = hash_u64(hash, id);
            }
            hash = hash_u64(hash, -2);
        }
        let initial = resolve_initial_rules(&group(2, right_start, right_start + 2, &right));
        for filtered in initial {
            for id in ids(&filtered) {
                hash = hash_u64(hash, id);
            }
            hash = hash_u64(hash, -3);
        }
    }
    assert_eq!(hash, 11_025_866_149_445_034_741);
}
