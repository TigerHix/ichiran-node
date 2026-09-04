use super::*;
use crate::dto::{
    AnalysisAlternative, AnalysisChunk, AnalysisPath, AnalysisRoot, AnalysisToken, PublicRoute,
};
use crate::morphology::Route;

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
