use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;

use super::*;
use crate::analyzer_lexicon::AnalyzerLexicon;
use crate::analyzer_model::PathPart;
use crate::annotations::AnalyzerAnnotations;
use crate::morphology::Morphology;
use crate::pack::Pack;
use crate::roots::RootPayload;
use crate::support::AnalyzerSupport;
use crate::surface::SurfaceIndex;
use crate::text::utf16;

fn release() -> PathBuf {
    std::env::var_os("ICHIRAN_M1_PACK_DIR")
        .map(PathBuf::from)
        .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory")
}

fn path(score: f64) -> PathResult {
    PathResult {
        score,
        parts: Vec::new(),
    }
}

#[test]
fn stable_cross_chunk_top_n_matches_typescript_generation_order() {
    let left = [
        AccumulatedPath {
            score: 10.0,
            word_paths: vec![ChunkPathRef {
                chunk_index: 0,
                path_index: 0,
            }],
        },
        AccumulatedPath {
            score: 10.0,
            word_paths: vec![ChunkPathRef {
                chunk_index: 0,
                path_index: 1,
            }],
        },
    ];
    let merged = merge_paths(&left, &[path(7.0), path(7.0), path(2.5)], 2, 4);
    assert_eq!(
        merged.iter().map(|value| value.score).collect::<Vec<_>>(),
        [17.0; 4]
    );
    assert_eq!(
        merged
            .iter()
            .map(|value| value.word_paths.clone())
            .collect::<Vec<_>>(),
        vec![
            vec![
                ChunkPathRef {
                    chunk_index: 0,
                    path_index: 0,
                },
                ChunkPathRef {
                    chunk_index: 2,
                    path_index: 0,
                },
            ],
            vec![
                ChunkPathRef {
                    chunk_index: 0,
                    path_index: 0,
                },
                ChunkPathRef {
                    chunk_index: 2,
                    path_index: 1,
                },
            ],
            vec![
                ChunkPathRef {
                    chunk_index: 0,
                    path_index: 1,
                },
                ChunkPathRef {
                    chunk_index: 2,
                    path_index: 0,
                },
            ],
            vec![
                ChunkPathRef {
                    chunk_index: 0,
                    path_index: 1,
                },
                ChunkPathRef {
                    chunk_index: 2,
                    path_index: 1,
                },
            ],
        ]
    );
    assert!(merge_paths(&left, &[path(1.0)], 1, 0).is_empty());
    assert!(merge_paths(&left, &[], 1, 5).is_empty());
}

#[test]
fn sticky_utf16_positions_match_typescript_character_rules() {
    assert_eq!(
        super::groups::sticky_positions(&utf16("った")),
        HashSet::from([1])
    );
    assert!(super::groups::sticky_positions(&utf16("かぁ")).is_empty());
    assert_eq!(
        super::groups::sticky_positions(&utf16("きぁ")),
        HashSet::from([1])
    );
    assert!(super::groups::sticky_positions(&utf16("カー")).is_empty());
    assert_eq!(
        super::groups::sticky_positions(&utf16("ーカゝ")),
        HashSet::from([0, 2])
    );
    assert!(super::groups::sticky_positions(&[0xd83d, 0xde00, 0xd83d]).is_empty());
}

#[test]
fn reachable_kanji_break_rules_match_typescript_exceptions() {
    let mut breaks = HashSet::new();
    super::groups::record_kanji_breaks(&utf16("です"), 4, 6, &mut breaks);
    assert_eq!(breaks, HashSet::from([5]));

    breaks.clear();
    super::groups::record_kanji_breaks(&utf16("日置"), 3, 5, &mut breaks);
    assert!(breaks.is_empty());

    super::groups::record_kanji_breaks(&utf16("漢字語"), 7, 10, &mut breaks);
    assert_eq!(breaks, HashSet::from([8, 9]));
}

#[test]
fn end_candidate_overwrite_preserves_javascript_map_order() {
    let mut values = super::groups::EndCandidates::new();
    values.set(4, Vec::new());
    values.set(9, Vec::new());
    values.set(4, Vec::new());
    values.append(6, Vec::new());
    assert_eq!(
        values.into_iter().map(|(end, _)| end).collect::<Vec<_>>(),
        [4, 9, 6]
    );
}

fn path_signature(path: &PathResult) -> (f64, Vec<(i64, usize, usize, f64)>) {
    let groups = path
        .parts
        .iter()
        .filter_map(|part| match part {
            PathPart::Group(group) => group.segments.first(),
            PathPart::Adjustment(_) => None,
        })
        .map(|segment| {
            (
                segment.candidate_id,
                segment.start,
                segment.end,
                segment.score,
            )
        })
        .collect();
    (path.score, groups)
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn typescript_group_and_path_oracle_is_exact_on_real_pack() {
    let pack = Pack::open(fs::read(release().join("hot.bin")).expect("read qualified hot.bin"))
        .expect("open qualified pack");
    let surface = SurfaceIndex::open(pack.section_data(1).unwrap()).unwrap();
    let roots = RootPayload::open(pack.section_data(2).unwrap()).unwrap();
    let morphology = Morphology::open(pack.section_data(3).unwrap()).unwrap();
    let support = AnalyzerSupport::open(pack.section_data(4).unwrap()).unwrap();
    let mut annotations = AnalyzerAnnotations::open(pack.section_data(5).unwrap()).unwrap();
    let mut lexicon =
        AnalyzerLexicon::new(&surface, &roots, &morphology, &support, &mut annotations);
    let mut engine = AnalyzerEngine::new(&surface, &support, &mut lexicon);

    let fixtures = [
        ("猫", vec![(19.0, vec![(1, 0, 1, 19.0)]), (-500.0, vec![])]),
        (
            "食べたい",
            vec![
                (378.0, vec![(5, 0, 4, 378.0)]),
                (128.0, vec![(3, 0, 2, 112.0), (26, 2, 4, 16.0)]),
                (-164.0, vec![(4, 0, 3, 336.0)]),
            ],
        ),
        (
            "3本",
            vec![
                (128.0, vec![(3, 0, 2, 128.0)]),
                (21.0, vec![(1, 0, 1, 5.0), (4, 1, 2, 16.0)]),
                (-484.0, vec![(4, 1, 2, 16.0)]),
            ],
        ),
        (
            "ネコ",
            vec![(32.0, vec![(1, 0, 2, 32.0)]), (-1000.0, vec![])],
        ),
        (
            "猫です",
            vec![
                (93.0, vec![(1, 0, 1, 19.0), (8, 1, 3, 64.0)]),
                (83.0, vec![(1, 0, 1, 19.0), (8, 1, 3, 64.0)]),
                (-436.0, vec![(8, 1, 3, 64.0)]),
            ],
        ),
        (
            "では",
            vec![
                (40.0, vec![(6, 0, 2, 40.0)]),
                (18.0, vec![(3, 0, 1, 11.0), (14, 1, 2, 16.0)]),
                (-484.0, vec![(14, 1, 2, 16.0)]),
            ],
        ),
    ];
    for (text, expected) in fixtures {
        engine.lexicon.reset();
        let actual = engine.analyze_word(&utf16(text), 3, &[]).unwrap();
        assert_eq!(
            actual.paths.iter().map(path_signature).collect::<Vec<_>>(),
            expected,
            "TypeScript group/path differential mismatch for {text}"
        );
    }

    engine.lexicon.reset();
    let entity = engine
        .analyze_word(
            &utf16("猫"),
            3,
            &[EntityHint {
                start: 0,
                end: 1,
                boost: Some(0.5),
            }],
        )
        .unwrap();
    assert_eq!(
        entity.paths.iter().map(path_signature).collect::<Vec<_>>(),
        vec![
            (19.5, vec![(1, 0, 1, 19.0)]),
            (1.0, vec![(-1, 0, 1, 0.5)]),
            (-500.0, vec![]),
        ]
    );
}
