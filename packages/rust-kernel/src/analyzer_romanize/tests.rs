use std::fs;
use std::path::PathBuf;

use super::*;
use crate::Kernel;
use crate::dto::{AnalysisPath, AnalysisResult, AnalysisToken, PublicRoute, Utf16Text};

fn u(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}

fn text(value: &[u16]) -> Utf16Text {
    Utf16Text::from_units(value)
}

fn token(value: &[u16], reading: &[u16]) -> AnalysisToken {
    AnalysisToken {
        candidate_id: Some(1),
        start: 0,
        end: value.len(),
        text: text(value),
        true_text: None,
        route: PublicRoute::Kana,
        reading: text(reading),
        romanized: text(&[]),
        pos: Vec::new(),
        score: 0.0,
        entry_index: None,
        root: None,
        inflection: Vec::new(),
        components: Vec::new(),
        alternatives: Vec::new(),
        skipped: 0,
        entity: false,
        counter: None,
        legacy: None,
    }
}

fn path(tokens: Vec<AnalysisToken>) -> AnalysisPath {
    AnalysisPath { score: 0.0, tokens }
}

fn result(
    normalized: &[u16],
    chunks: Vec<AnalysisChunk>,
    paths: Vec<AnalysisPath>,
) -> AnalysisResult {
    AnalysisResult {
        input: text(normalized),
        normalized: text(normalized),
        compute_ms: 0.0,
        chunks,
        paths,
    }
}

#[test]
fn uses_each_chunks_first_path_and_preserves_misc_parts() {
    let cat = token(&u("猫"), &u("ねこ"));
    let ignored = token(&u("猫"), &u("びょう"));
    let analysis = result(
        &u("猫。猫"),
        vec![
            AnalysisChunk::Word {
                start: 0,
                end: 1,
                text: text(&u("猫")),
                paths: vec![path(vec![cat.clone()]), path(vec![ignored.clone()])],
            },
            AnalysisChunk::Misc {
                start: 1,
                end: 2,
                text: text(&u("。")),
            },
            AnalysisChunk::Word {
                start: 2,
                end: 3,
                text: text(&u("猫")),
                paths: vec![path(vec![cat]), path(vec![ignored])],
            },
        ],
        vec![path(Vec::new())],
    );
    assert_eq!(
        romanize_analysis(&analysis, RomanizationName::HepburnTraditional),
        u("neko。 neko")
    );
}

#[test]
fn applies_all_methods_and_original_spelling_specials() {
    let expected = [
        (RomanizationName::HepburnBasic, "toukyou"),
        (RomanizationName::HepburnSimple, "tokyo"),
        (RomanizationName::HepburnPassport, "tohkyoh"),
        (RomanizationName::HepburnTraditional, "tōkyō"),
        (RomanizationName::HepburnModified, "tōkyō"),
        (RomanizationName::KunreiSiki, "tôkyô"),
    ];
    for (method, output) in expected {
        let analysis = result(
            &u("東京"),
            vec![AnalysisChunk::Word {
                start: 0,
                end: 2,
                text: text(&u("東京")),
                paths: vec![path(vec![token(&u("東京"), &u("とうきょう"))])],
            }],
            vec![path(Vec::new())],
        );
        assert_eq!(romanize_analysis(&analysis, method), u(output));
    }

    for (original, expected) in [("っ", "!"), ("ー", "~")] {
        let analysis = result(
            &u(original),
            vec![AnalysisChunk::Word {
                start: 0,
                end: 1,
                text: text(&u(original)),
                paths: vec![path(vec![token(&u(original), &u("かな"))])],
            }],
            vec![path(Vec::new())],
        );
        assert_eq!(
            romanize_analysis(&analysis, RomanizationName::HepburnTraditional),
            u(expected)
        );
    }
}

#[test]
fn no_global_path_uses_normalized_fallback_and_utf16_is_lossless() {
    let fallback = result(&u("トウキョウ"), Vec::new(), Vec::new());
    assert_eq!(
        romanize_analysis(&fallback, RomanizationName::HepburnTraditional),
        u("tōkyō")
    );

    for units in [vec![0xd83d, 0xde00], vec![0xd83d], vec![0xde00]] {
        let analysis = result(
            &units,
            vec![AnalysisChunk::Misc {
                start: 0,
                end: units.len(),
                text: text(&units),
            }],
            vec![path(Vec::new())],
        );
        assert_eq!(
            romanize_analysis(&analysis, RomanizationName::HepburnTraditional),
            units
        );
    }
}

#[test]
fn empty_word_path_matches_typescript_flat_map_behavior() {
    let analysis = result(
        &u("猫"),
        vec![AnalysisChunk::Word {
            start: 0,
            end: 1,
            text: text(&u("猫")),
            paths: Vec::new(),
        }],
        vec![path(Vec::new())],
    );
    assert!(romanize_analysis(&analysis, RomanizationName::HepburnTraditional).is_empty());
}

fn release() -> PathBuf {
    std::env::var_os("ICHIRAN_M1_PACK_DIR")
        .map(PathBuf::from)
        .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory")
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn five_authoritative_standalone_results_are_exact() {
    let mut kernel = Kernel::open(fs::read(release().join("hot.bin")).expect("read hot.bin"))
        .expect("open qualified pack");
    for (input, expected) in [
        ("こんにちは", "konnichiwa"),
        ("今日はいい天気です", "kyō wa iitenki desu"),
        ("ご注文はうさぎですか", "gochūmon wa usagi desu ka"),
        ("土足で", "dosokude"),
        ("みんな土足でおいで", "minna dosokude oide"),
    ] {
        let analysis = kernel.analyze_str(input, 1).unwrap();
        assert_eq!(
            romanize_analysis(&analysis, RomanizationName::HepburnTraditional),
            u(expected),
            "standalone romanization mismatch for {input}"
        );
    }
}
