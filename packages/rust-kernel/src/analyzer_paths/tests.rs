use std::cell::Cell;

use super::*;
use crate::analyzer_model::{ScoreBreakdown, ScoreInfo, utf16};

fn portable_group(group_id: i64, start: usize, end: usize, scores: &[f64]) -> SegmentGroup {
    SegmentGroup {
        group_id,
        start,
        end,
        segments: scores
            .iter()
            .enumerate()
            .map(|(index, score)| Segment {
                candidate_id: group_id * 10 + index as i64,
                start,
                end,
                score: *score,
                common: None,
                entity: false,
                rules: None,
            })
            .collect(),
        matches: scores.len(),
    }
}

fn signature(parts: &[PathPart]) -> Vec<(usize, usize, f64)> {
    parts
        .iter()
        .filter_map(|part| match part {
            PathPart::Group(group) => Some((group.start, group.end, group_score(group))),
            PathPart::Adjustment(_) => None,
        })
        .collect()
}

#[test]
fn neutral_transition_dp_matches_typescript_reference_fixture() {
    let groups = vec![
        portable_group(0, 0, 1, &[70.0, 60.0]),
        portable_group(1, 0, 2, &[130.0]),
        portable_group(2, 1, 3, &[115.0]),
        portable_group(3, 2, 4, &[125.0]),
        portable_group(4, 4, 5, &[40.0]),
    ];
    let entities = [EntityHint {
        start: 0,
        end: 2,
        boost: Some(25.0),
    }];
    let paths = find_paths(&groups, 5, 5, &entities).unwrap();
    assert_eq!(
        paths.iter().map(|path| path.score).collect::<Vec<_>>(),
        [320.0, -220.0, -265.0, -275.0, -805.0]
    );
    assert_eq!(
        paths
            .iter()
            .map(|path| signature(&path.parts))
            .collect::<Vec<_>>(),
        [
            vec![(0, 2, 130.0), (2, 4, 125.0), (4, 5, 40.0)],
            vec![(0, 2, 130.0), (2, 4, 125.0)],
            vec![(0, 1, 70.0), (2, 4, 125.0), (4, 5, 40.0)],
            vec![(0, 1, 70.0), (1, 3, 115.0), (4, 5, 40.0)],
            vec![(0, 1, 70.0), (2, 4, 125.0)],
        ]
    );
}

#[test]
fn equal_scores_keep_insertion_order_and_exact_limit() {
    let groups = vec![
        portable_group(10, 0, 1, &[100.0]),
        portable_group(11, 0, 1, &[100.0]),
        portable_group(12, 0, 1, &[100.0]),
    ];
    let paths = find_paths(&groups, 1, 2, &[]).unwrap();
    assert_eq!(paths.len(), 2);
    assert_eq!(
        paths
            .iter()
            .map(|path| match &path.parts[0] {
                PathPart::Group(group) => group.group_id,
                PathPart::Adjustment(_) => unreachable!(),
            })
            .collect::<Vec<_>>(),
        [10, 11]
    );
}

#[test]
fn filtered_pair_replacements_and_adjustment_score_match_typescript() {
    let left = portable_group(20, 0, 1, &[20.0, 10.0]);
    let right = portable_group(21, 1, 2, &[18.0, 7.0]);
    let mut filtered_left = left.clone();
    filtered_left.segments = vec![left.segments[1].clone()];
    let mut filtered_right = right.clone();
    filtered_right.segments = vec![right.segments[1].clone()];
    let transition = |_: &SegmentGroup, _: &SegmentGroup| {
        vec![PathTransition {
            right: filtered_right.clone(),
            adjustment: Some(crate::analyzer_model::PathAdjustment {
                score: 50.0,
                start: 1,
                end: 1,
                description: "fixture".to_owned(),
                connector: " ".to_owned(),
            }),
            left: filtered_left.clone(),
        }]
    };
    let paths = find_paths_with(&[left, right], 2, 3, &[], None, Some(&transition)).unwrap();
    assert_eq!(paths[0].score, 67.0);
    assert_eq!(
        paths[0]
            .parts
            .iter()
            .map(|part| match part {
                PathPart::Group(group) => group.group_id as f64,
                PathPart::Adjustment(adjustment) => adjustment.score,
            })
            .collect::<Vec<_>>(),
        [20.0, 50.0, 21.0]
    );
}

#[test]
fn synthetic_entity_boost_is_intentionally_applied_twice() {
    let entity = EntityHint {
        start: 0,
        end: 2,
        boost: Some(75.0),
    };
    let groups = add_entity_groups(&[], &[entity], &[]);
    assert_eq!(groups[0].segments[0].score, 75.0);
    let paths = find_paths(&groups, 2, 5, &[entity]).unwrap();
    assert_eq!(paths[0].score, 150.0);
    assert!(matches!(
        &paths[0].parts[0],
        PathPart::Group(group) if group.segments[0].entity
    ));
}

#[test]
fn fractional_and_negative_entity_boosts_match_typescript_numbers() {
    let group = portable_group(1, 0, 1, &[100.0]);
    let fractional = find_paths(
        &[group],
        1,
        1,
        &[EntityHint {
            start: 0,
            end: 1,
            boost: Some(2.5),
        }],
    )
    .unwrap();
    assert_eq!(fractional[0].score, 102.5);

    let entity = EntityHint {
        start: 0,
        end: 2,
        boost: Some(-3.25),
    };
    let synthetic = add_entity_groups(&[], &[entity], &[]);
    assert_eq!(
        find_paths(&synthetic, 2, 1, &[entity]).unwrap()[0].score,
        -6.5
    );
    assert_eq!(
        find_paths(
            &[],
            1,
            1,
            &[EntityHint {
                start: 0,
                end: 1,
                boost: Some(f64::NAN),
            }],
        )
        .unwrap_err(),
        "entities[0].boost must be finite and between -1000000 and 1000000"
    );
}

#[test]
fn allocation_sized_limit_is_rejected_before_top_n_buffers() {
    assert_eq!(
        find_paths(&[], 0, 100_000_000, &[]).unwrap_err(),
        "limit must be an integer from 1 to 10"
    );
}

#[test]
fn dense_graph_materializes_only_the_final_long_path() {
    let group_count = 2_000;
    let groups: Vec<_> = (0..group_count)
        .map(|index| portable_group(index as i64, index, index + 1, &[1.0]))
        .collect();
    let transitions = Cell::new(0);
    let initial = |group: &SegmentGroup| vec![group.clone()];
    let transition = |left: &SegmentGroup, right: &SegmentGroup| {
        transitions.set(transitions.get() + 1);
        vec![PathTransition {
            left: left.clone(),
            right: right.clone(),
            adjustment: None,
        }]
    };
    let paths = find_paths_with(
        &groups,
        group_count,
        1,
        &[],
        Some(&initial),
        Some(&transition),
    )
    .unwrap();
    assert_eq!(transitions.get(), group_count * (group_count - 1) / 2);
    assert_eq!(paths[0].parts.len(), group_count);
}

fn add_random_rule_facts(group: &mut SegmentGroup, random: &mut impl FnMut() -> u32) {
    let strong = random().is_multiple_of(3);
    for (index, segment) in group.segments.iter_mut().enumerate() {
        segment.rules = Some(SegmentRuleFacts {
            text: utf16(if index == 0 && random().is_multiple_of(5) {
                "と"
            } else {
                "あ"
            }),
            word_kind: RuleWordKind::Simple,
            score_info: Some(ScoreInfo {
                positions: Vec::new(),
                seq_set: if random().is_multiple_of(7) {
                    vec![1_342_560]
                } else {
                    Vec::new()
                },
                conjugations: Vec::new(),
                common: None,
                breakdown: ScoreBreakdown {
                    property_score: 1,
                    kanji_break: None,
                    use_length_bonus: 0,
                    split: None,
                },
                flags: if strong {
                    crate::analyzer_model::SCORE_FLAG_STRONG
                } else {
                    0
                },
            }),
            compound_end_seq: None,
            compound_end_text: None,
        });
    }
}

#[test]
fn optimized_default_rules_match_exhaustive_dense_gap_graph() {
    let mut state = 0x51ee_71e5_u32;
    let mut random = || {
        state = state.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
        state
    };
    for _ in 0..80 {
        let text_length = 12 + random() as usize % 9;
        let mut groups = Vec::new();
        let mut group_id = 1;
        for start in 0..text_length {
            for length in 1..=4 {
                if start + length > text_length {
                    break;
                }
                if random() % 4 == 0 {
                    continue;
                }
                let scores = [f64::from(5 + random() % 8), f64::from(5 + random() % 8)];
                let mut group = portable_group(group_id, start, start + length, &scores);
                group_id += 1;
                add_random_rule_facts(&mut group, &mut random);
                groups.push(group);
            }
        }
        let entities = [EntityHint {
            start: random() as usize % (text_length - 1),
            end: text_length,
            boost: Some(f64::from(random() % 5) - 2.0),
        }];
        let optimized = find_paths(&groups, text_length, 10, &entities).unwrap();
        let transition =
            |left: &SegmentGroup, right: &SegmentGroup| resolve_rule_transitions(left, right);
        let exhaustive =
            find_paths_with(&groups, text_length, 10, &entities, None, Some(&transition)).unwrap();
        assert_eq!(optimized, exhaustive);
    }
}

#[test]
fn custom_initial_span_keeps_exhaustive_gap_semantics() {
    let groups = vec![
        portable_group(1, 0, 1, &[10.0]),
        portable_group(2, 2, 3, &[10.0]),
    ];
    let initial = |group: &SegmentGroup| {
        let mut group = group.clone();
        if group.group_id == 1 {
            group.end = 2;
        }
        vec![group]
    };
    let actual = find_paths_with(&groups, 3, 5, &[], Some(&initial), None).unwrap();
    let transition =
        |left: &SegmentGroup, right: &SegmentGroup| resolve_rule_transitions(left, right);
    let exhaustive =
        find_paths_with(&groups, 3, 5, &[], Some(&initial), Some(&transition)).unwrap();
    assert_eq!(actual, exhaustive);
}
