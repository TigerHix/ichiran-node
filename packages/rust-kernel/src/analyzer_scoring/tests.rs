use super::*;
use crate::analyzer_model::{
    CompoundScoreFacts, ConjugationProperty, EntryScoreFacts, SequenceFacts, SuruBreakFacts, utf16,
};

fn active_sequence() -> SequenceFacts {
    SequenceFacts::default()
}

fn word(text: &str) -> WordScoreFacts {
    WordScoreFacts {
        kind: ScoreWordKind::Word,
        text: utf16(text),
        true_text: utf16(text),
        true_text_follows_text: true,
        route: Route::Kana,
        seq: Some(1_000_001),
        ord: 0,
        common: None,
        nokanji: false,
        entry: Some(EntryScoreFacts {
            root: true,
            n_kanji: 0,
            primary_nokanji: false,
        }),
        conjugation_only: false,
        conjugations: Vec::new(),
        positions: vec!["n".to_owned()],
        self_facts: active_sequence(),
        lineage: active_sequence(),
        inherited_common: None,
        inherited_ord: None,
        split: None,
        suru_break: None,
    }
}

fn candidate(word: WordScoreFacts) -> ScoreCandidate {
    ScoreCandidate::Word(word)
}

fn conjugation() -> Conjugation {
    Conjugation {
        seq: 2_000_001,
        from: 1_000_001,
        via: None,
        property: ConjugationProperty {
            pos: "v1".to_owned(),
            kind: 1,
            negative: Some(false),
            formal: Some(false),
        },
    }
}

fn segment(candidate_id: i64, score: f64, common: Option<i32>) -> Segment {
    Segment {
        candidate_id,
        start: 0,
        end: 1,
        score,
        common,
        entity: false,
        rules: None,
    }
}

#[test]
fn representative_root_weak_particle_and_skip_scores_match_typescript() {
    assert_eq!(mora_length(&[0xd83d, 0xde00]), 1);
    assert_eq!(mora_length(&[0xd83d]), 1);

    let mut strong = word("日本語");
    strong.route = Route::Kanji;
    strong.common = Some(1);
    strong.entry = Some(EntryScoreFacts {
        root: true,
        n_kanji: 1,
        primary_nokanji: false,
    });
    let strong = score_candidate(&candidate(strong), &ScoreOptions::default());
    assert_eq!(strong.score, 1088);
    assert_eq!(strong.info.breakdown.property_score, 32);
    assert_eq!(strong.info.breakdown.kanji_break, None);
    assert_eq!(strong.info.breakdown.use_length_bonus, 0);
    assert_eq!(strong.info.breakdown.split, None);
    assert_eq!(
        strong.info.flags,
        SCORE_FLAG_STRONG | SCORE_FLAG_PRIMARY | SCORE_FLAG_COMMON | SCORE_FLAG_LONG
    );

    let mut weak = word("たべ");
    weak.seq = Some(2_000_001);
    weak.ord = 3;
    weak.entry = Some(EntryScoreFacts {
        root: false,
        n_kanji: 1,
        primary_nokanji: false,
    });
    weak.conjugation_only = true;
    let mut weak_conjugation = conjugation();
    weak_conjugation.property.pos = "adj-i".to_owned();
    weak_conjugation.property.kind = 51;
    weak.conjugations = vec![weak_conjugation];
    weak.positions = vec!["adj-i".to_owned()];
    weak.inherited_common = Some(1);
    weak.inherited_ord = Some(0);
    let weak = score_candidate(&candidate(weak), &ScoreOptions::default());
    assert_eq!(weak.score, 4);
    assert_eq!(weak.info.common, Some(1));
    assert_eq!(weak.info.flags, SCORE_FLAG_COMMON);

    let mut particle = word("ね");
    particle.seq = Some(2_029_080);
    particle.common = Some(0);
    particle.positions = vec!["prt".to_owned()];
    let particle = candidate(particle);
    assert_eq!(
        score_candidate(&particle, &ScoreOptions::default()).score,
        6
    );
    assert_eq!(
        score_candidate(
            &particle,
            &ScoreOptions {
                final_word: true,
                ..ScoreOptions::default()
            }
        )
        .score,
        16
    );

    let mut final_only = word("かい");
    final_only.seq = Some(2_017_770);
    let final_only = candidate(final_only);
    assert_eq!(
        score_candidate(&final_only, &ScoreOptions::default()).score,
        0
    );
    assert!(
        score_candidate(
            &final_only,
            &ScoreOptions {
                final_word: true,
                ..ScoreOptions::default()
            }
        )
        .score
            > 0
    );
    for (seq, skipped) in [(2_458_040, false), (2_827_357, true)] {
        let mut fixture = word("かな");
        fixture.seq = Some(seq);
        assert_eq!(
            score_candidate(&candidate(fixture), &ScoreOptions::default()).score == 0,
            skipped
        );
    }
}

#[test]
fn compound_modifier_and_split_arithmetic_matches_typescript() {
    let mut base = word("テスト");
    base.common = Some(1);
    assert_eq!(
        score_candidate(&candidate(base.clone()), &ScoreOptions::default()).score,
        720
    );
    let compound = ScoreCandidate::Compound(CompoundScoreFacts {
        text: utf16("テストです"),
        base: Box::new(candidate(base)),
        modifier: ScoreModifier {
            multiplier: 2,
            constant: 7,
        },
        conjugations: Vec::new(),
        suru_break: None,
    });
    let compound = score_candidate(&compound, &ScoreOptions::default());
    assert_eq!(compound.score, 1117);
    assert_eq!(compound.info.breakdown.use_length_bonus, 397);

    let mut additive = word("日本語");
    additive.route = Route::Kanji;
    additive.common = Some(1);
    additive.entry = Some(EntryScoreFacts {
        root: true,
        n_kanji: 1,
        primary_nokanji: false,
    });
    additive.split = Some(ScoreSplit::Add(-88));
    let additive = score_candidate(&candidate(additive), &ScoreOptions::default());
    assert_eq!(additive.score, 1000);
    assert_eq!(
        additive.info.breakdown.split,
        Some(SplitScoreInfo::Add(-88))
    );

    let mut proportional = word("日本語");
    proportional.route = Route::Kanji;
    proportional.common = Some(1);
    proportional.entry = Some(EntryScoreFacts {
        root: true,
        n_kanji: 1,
        primary_nokanji: false,
    });
    proportional.split = Some(ScoreSplit::Proportional(-2));
    let proportional = score_candidate(&candidate(proportional), &ScoreOptions::default());
    assert_eq!(proportional.score, 1020);
    assert_eq!(proportional.info.breakdown.property_score, 30);
    assert_eq!(proportional.info.breakdown.split, None);

    let mut parts = word("かなかな");
    parts.split = Some(ScoreSplit::Parts {
        score: 5,
        parts: vec![candidate(word("かな")), candidate(word("かな"))],
        truncated_last: None,
    });
    let parts = score_candidate(&candidate(parts), &ScoreOptions::default());
    assert_eq!(parts.score, 37);
    assert_eq!(
        parts.info.breakdown.split,
        Some(SplitScoreInfo::Parts(vec![5, 16, 16]))
    );
}

#[test]
fn kanji_break_exemptions_endpoint_bonuses_and_floor_match_typescript() {
    let mut ordinary = word("日本語");
    ordinary.route = Route::Kanji;
    ordinary.common = Some(1);
    ordinary.entry = Some(EntryScoreFacts {
        root: true,
        n_kanji: 1,
        primary_nokanji: false,
    });
    assert_eq!(
        score_candidate(
            &candidate(ordinary),
            &ScoreOptions {
                kanji_break: Some(vec![1]),
                ..ScoreOptions::default()
            }
        )
        .score,
        544
    );

    let mut prefix = word("日本");
    prefix.route = Route::Kanji;
    prefix.positions = vec!["pref".to_owned()];
    let prefix = candidate(prefix);
    let raw = score_candidate(&prefix, &ScoreOptions::default()).score;
    assert_eq!(
        score_candidate(
            &prefix,
            &ScoreOptions {
                kanji_break: Some(vec![1]),
                ..ScoreOptions::default()
            }
        )
        .score,
        SCORE_CUTOFF.max((raw + 1) / 2 + 12)
    );

    let mut exempt = word("飲む");
    exempt.route = Route::Kanji;
    exempt.seq = Some(1_169_870);
    exempt.entry = Some(EntryScoreFacts {
        root: true,
        n_kanji: 1,
        primary_nokanji: false,
    });
    let exempt = candidate(exempt);
    assert_eq!(
        score_candidate(
            &exempt,
            &ScoreOptions {
                kanji_break: Some(vec![1]),
                ..ScoreOptions::default()
            }
        )
        .score,
        score_candidate(&exempt, &ScoreOptions::default()).score
    );

    let mut suffix = word("する");
    suffix.positions = vec!["vs-i".to_owned()];
    let suffix = candidate(suffix);
    let mut compound = word("日本語する");
    compound.route = Route::Kanji;
    compound.common = Some(1);
    compound.positions = vec!["vs-s".to_owned()];
    compound.entry = Some(EntryScoreFacts {
        root: true,
        n_kanji: 1,
        primary_nokanji: false,
    });
    compound.suru_break = Some(SuruBreakFacts {
        suffix_text: utf16("する"),
        candidate: Box::new(suffix.clone()),
    });
    assert_eq!(
        score_candidate(
            &candidate(compound),
            &ScoreOptions {
                kanji_break: Some(vec![1]),
                ..ScoreOptions::default()
            }
        )
        .score,
        score_candidate(&suffix, &ScoreOptions::default()).score + 50
    );
}

#[test]
fn counter_root_and_position_floor_matches_typescript() {
    let mut counter = word("三人");
    counter.kind = ScoreWordKind::Counter;
    counter.route = Route::Kanji;
    counter.common = Some(1);
    counter.entry = Some(EntryScoreFacts {
        root: false,
        n_kanji: 1,
        primary_nokanji: false,
    });
    let counter = score_candidate(&candidate(counter), &ScoreOptions::default());
    assert_eq!(counter.score, 325);
    assert_eq!(counter.info.positions, ["ctr"]);
}

#[test]
fn stable_common_and_score_order_matches_typescript() {
    let common = [None, Some(0), Some(1), Some(2), Some(9)];
    let expected = [
        [false, false, false, false, false],
        [true, false, false, false, false],
        [true, true, false, true, true],
        [true, true, false, false, true],
        [true, true, false, false, false],
    ];
    for (left_index, left) in common.iter().enumerate() {
        for (right_index, right) in common.iter().enumerate() {
            assert_eq!(
                compare_common(*left, *right),
                expected[left_index][right_index]
            );
        }
    }
    let input = vec![
        segment(0, 30.0, None),
        segment(1, 30.0, Some(0)),
        segment(2, 30.0, Some(2)),
        segment(3, 30.0, Some(1)),
        segment(4, 15.0, Some(1)),
        segment(5, 14.0, Some(1)),
        segment(6, 31.0, None),
    ];
    assert_eq!(
        cull_segments(&input)
            .iter()
            .map(|item| item.candidate_id)
            .collect::<Vec<_>>(),
        [6, 3, 2, 1, 0]
    );
}

#[test]
fn lookup_and_presentation_cutoffs_keep_legacy_order() {
    let input = vec![
        segment(0, 10.0, None),
        segment(1, 6.7, None),
        segment(2, 5.0, None),
        segment(3, 4.0, None),
    ];
    assert_eq!(
        filter_and_cull_segments(&input)
            .iter()
            .map(|item| item.candidate_id)
            .collect::<Vec<_>>(),
        [0, 1, 2]
    );
    assert_eq!(
        select_alternatives(&input)
            .iter()
            .map(|item| item.candidate_id)
            .collect::<Vec<_>>(),
        [0, 1]
    );
    for start in 0..5 {
        for end in start..8 {
            assert_eq!(
                crate::analyzer_paths::gap_penalty(start, end),
                -500.0 * (end - start) as f64
            );
        }
    }
}
