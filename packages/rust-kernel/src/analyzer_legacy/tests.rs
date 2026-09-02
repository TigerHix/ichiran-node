use std::fs;
use std::path::PathBuf;

use serde_json::Value;

use super::*;
use crate::annotations::AnalyzerAnnotations;
use crate::details::DetailStore;
use crate::dto::{
    AnalysisAlternative, AnalysisChunk, AnalysisPath, AnalysisRoot, AnalysisToken, PublicRoute,
};
use crate::morphology::Route;
use crate::pack::Pack;
use crate::roots::RootPayload;
use crate::support::AnalyzerSupport;
use crate::surface::SurfaceIndex;

fn text(value: &str) -> Utf16Text {
    Utf16Text::from(value)
}

fn root(seq: u32, form: &str, reading: &str) -> AnalysisRoot {
    AnalysisRoot {
        seq,
        form: form.to_owned(),
        reading: reading.to_owned(),
    }
}

fn alternative(
    candidate_id: i64,
    value: &str,
    reading: &str,
    seq: u32,
    score: f64,
    counter: Option<(String, bool)>,
) -> AnalysisAlternative {
    AnalysisAlternative {
        candidate_id,
        text: text(value),
        true_text: None,
        route: Route::Kanji,
        reading: text(reading),
        romanized: text(""),
        pos: Vec::new(),
        score,
        entry_index: Some(seq as usize),
        root: Some(root(seq, value, reading)),
        inflection: Vec::new(),
        components: Vec::new(),
        counter,
        legacy: None,
    }
}

fn base_token(value: &str, reading: &str, start: usize, end: usize) -> AnalysisToken {
    AnalysisToken {
        candidate_id: Some(1),
        start,
        end,
        text: text(value),
        true_text: None,
        route: PublicRoute::Kanji,
        reading: text(reading),
        romanized: text(""),
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

#[test]
fn compact_serialization_matches_typescript_nesting_order_and_contextual_reading() {
    let counter = Some(("Value: 6".to_owned(), false));
    let mut multiple = base_token("6本", "ろっぽん", 0, 2);
    multiple.score = 128.0;
    multiple.skipped = 2;
    multiple.counter = counter.clone();
    multiple.alternatives = vec![
        alternative(1, "6本", "ろっぽん", 1_522_150, 128.0, counter.clone()),
        alternative(2, "6本", "ろくもと", 1_260_670, 88.0, counter),
    ];

    let mut nani = base_token("何", "なに", 2, 3);
    nani.score = 50.0;
    nani.alternatives = vec![
        alternative(3, "何", "なん", 3, 50.0, None),
        alternative(4, "何", "なに", 4, 50.0, None),
    ];
    nani.legacy = Some(LegacyPresentationFacts {
        physical_group: None,
        suffix_class: None,
        definition_seq: Some(3),
        semantic_members: Vec::new(),
        identity_roots: Vec::new(),
        conjugation_selection: LegacyConjugationSelection::Default,
        contextual_reading: true,
    });
    let path = AnalysisPath {
        score: 178.0,
        tokens: vec![multiple, nani],
    };
    let result = AnalysisResult {
        input: text("6本何。"),
        normalized: text("6本何。"),
        compute_ms: 0.0,
        chunks: vec![
            AnalysisChunk::Word {
                start: 0,
                end: 3,
                text: text("6本何"),
                paths: vec![path],
            },
            AnalysisChunk::Misc {
                start: 3,
                end: 4,
                text: text("。"),
            },
        ],
        paths: Vec::new(),
    };

    let serialized =
        serde_json::to_string(&serialize_compact(&result, &LegacyOptions::default())).unwrap();
    assert_eq!(
        serialized,
        include_str!("../../tests/fixtures/m3-legacy-compact.json").trim()
    );
}

#[test]
fn compact_honors_romanization_method_and_word_property_callback() {
    let mut token = base_token("し", "し", 0, 1);
    token.route = PublicRoute::Kana;
    let path = AnalysisPath {
        score: 1.0,
        tokens: vec![token],
    };
    let result = AnalysisResult {
        input: text("し"),
        normalized: text("し"),
        compute_ms: 0.0,
        chunks: vec![AnalysisChunk::Word {
            start: 0,
            end: 1,
            text: text("し"),
            paths: vec![path],
        }],
        paths: Vec::new(),
    };
    let property = |romanized: &[u16], _token: &AnalysisToken| serde_json::json!({ "romanized": String::from_utf16(romanized).unwrap() });
    let output = serialize_compact(
        &result,
        &LegacyOptions {
            method: Some(RomanizationName::KunreiSiki),
            word_property: Some(&property),
        },
    );
    assert_eq!(
        serde_json::to_value(output).unwrap()[0][0][0][0],
        serde_json::json!(["si", {
            "type": "KANA", "text": "し", "truetext": "し", "kana": "し",
            "score": 0, "start": 0, "end": 1, "skipped": 0
        }, { "romanized": "si" }])
    );
}

fn release() -> PathBuf {
    std::env::var_os("ICHIRAN_M1_PACK_DIR")
        .map(PathBuf::from)
        .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory")
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn detailed_serialization_retries_exact_ranges_and_matches_typescript_counter_and_conjugation() {
    let directory = release();
    let pack = Pack::open(fs::read(directory.join("hot.bin")).unwrap()).unwrap();
    let surface = SurfaceIndex::open(pack.section_data(1).unwrap()).unwrap();
    let roots = RootPayload::open(pack.section_data(2).unwrap()).unwrap();
    let support = AnalyzerSupport::open(pack.section_data(4).unwrap()).unwrap();
    let mut annotations = AnalyzerAnnotations::open(pack.section_data(5).unwrap()).unwrap();
    let cold = fs::read(directory.join("details.bin")).unwrap();
    let prefix_length = DetailStore::prefix_length(&cold[..96], cold.len()).unwrap();
    let details = DetailStore::open(cold[..prefix_length].to_vec(), cold.len()).unwrap();
    let fixture: Value =
        serde_json::from_str(include_str!("../../tests/fixtures/m3-fallback.json")).unwrap();
    let serialized_fixtures = include_str!("../../tests/fixtures/m3-legacy-detailed.txt")
        .lines()
        .collect::<Vec<_>>();

    let witnesses = [
        (
            fixture["suites"]["counters"][0]["detailed"].clone(),
            result_for(
                &roots,
                "1倍",
                "いちばい",
                1473230,
                "倍",
                "ばい",
                136.0,
                Vec::new(),
                Some(("Value: 1".to_owned(), false)),
            ),
        ),
        (
            fixture["suites"]["probes"][30]["detailed"].clone(),
            result_for(
                &roots,
                "食べた",
                "たべた",
                1358280,
                "食べる",
                "たべる",
                336.0,
                vec![MorphologyProperty {
                    pos: "v1".to_owned(),
                    kind: 2,
                    negative: Some(false),
                    formal: Some(false),
                    ordinal: 1,
                }],
                None,
            ),
        ),
    ];

    for ((expected, result), expected_serialized) in witnesses.into_iter().zip(serialized_fixtures)
    {
        let mut session = LegacyDetailedSession::default();
        let mut requested = Vec::new();
        let actual = loop {
            let mut context = LegacyContext {
                roots: &roots,
                support: &support,
                surface: &surface,
                annotations: &mut annotations,
            };
            match session
                .serialize(&result, &details, &mut context, &LegacyOptions::default())
                .unwrap()
            {
                LegacyDetailedResult::Ready(value) => break value,
                LegacyDetailedResult::MissingDetail(request) => {
                    requested.push(request);
                    let start = request.range.offset as usize;
                    let end = start + request.range.byte_length as usize;
                    details
                        .entry_from_compressed(request.entry_index, &cold[start..end])
                        .unwrap();
                }
            }
        };
        assert!(!requested.is_empty());
        assert!(
            requested.iter().all(|request| {
                (request.range.byte_length as usize) < cold.len() - prefix_length
            })
        );
        assert_eq!(serde_json::to_value(&actual).unwrap(), expected);
        assert_eq!(serde_json::to_string(&actual).unwrap(), expected_serialized);
    }
}

#[allow(clippy::too_many_arguments)]
fn result_for(
    roots: &RootPayload,
    value: &str,
    reading: &str,
    seq: u32,
    form: &str,
    root_reading: &str,
    score: f64,
    inflection: Vec<MorphologyProperty>,
    counter: Option<(String, bool)>,
) -> AnalysisResult {
    let mut token = base_token(value, reading, 0, value.encode_utf16().count());
    token.score = score;
    token.entry_index = roots.find_entry_index(seq).unwrap();
    token.root = Some(root(seq, form, root_reading));
    token.inflection = inflection;
    token.counter = counter;
    let path = AnalysisPath {
        score,
        tokens: vec![token],
    };
    AnalysisResult {
        input: text(value),
        normalized: text(value),
        compute_ms: 0.0,
        chunks: vec![AnalysisChunk::Word {
            start: 0,
            end: value.encode_utf16().count(),
            text: text(value),
            paths: vec![path.clone()],
        }],
        paths: vec![path],
    }
}
