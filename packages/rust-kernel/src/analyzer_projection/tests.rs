use serde_json::{Value, json};

use super::*;
use crate::analyzer_lexicon::{
    AnalysisRoot as LexiconRoot, ConjugationSelection, PhysicalKey, SemanticMember,
};
use crate::analyzer_model::{
    ScoreBreakdown, ScoreCandidate, ScoreWordKind, SequenceFacts, WordScoreFacts,
};
use crate::morphology::Route;

fn u(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}

fn score_candidate(
    candidate: MaterializedCandidate,
    positions: &[&str],
) -> ProjectionScoredCandidate {
    ProjectionScoredCandidate {
        candidate,
        info: ScoreInfo {
            positions: positions.iter().map(|value| (*value).to_owned()).collect(),
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
        },
    }
}

fn candidate(kind: CandidateKind, text: &str, reading: &str, seq: u32) -> MaterializedCandidate {
    let text = u(text);
    let reading = u(reading);
    let root = LexiconRoot {
        seq,
        form: text.clone(),
        reading: reading.clone(),
    };
    MaterializedCandidate {
        kind,
        text: text.clone(),
        true_text: text.clone(),
        route: Route::Kanji,
        reading,
        public_seq: Some(seq),
        physical_seq: Some(i64::from(seq)),
        physical_key: PhysicalKey::Sequence(seq),
        physical_group: None,
        lookup_locators: Vec::new(),
        member_ord: None,
        entry_index: Some(seq as usize),
        root: Some(root),
        inflection: Vec::new(),
        score_facts: ScoreCandidate::Word(WordScoreFacts {
            kind: ScoreWordKind::Word,
            text: text.clone(),
            true_text: text,
            true_text_follows_text: true,
            route: Route::Kanji,
            seq: Some(i64::from(seq)),
            ord: 0,
            common: None,
            nokanji: false,
            entry: None,
            conjugation_only: false,
            conjugations: Vec::new(),
            positions: Vec::new(),
            self_facts: SequenceFacts::default(),
            lineage: SequenceFacts::default(),
            inherited_common: None,
            inherited_ord: None,
            split: None,
            suru_break: None,
        }),
        components: Vec::new(),
        counter: None,
        suffix_class: None,
        definition_seq: None,
        semantic_members: Vec::<SemanticMember>::new(),
        identity_roots: Vec::new(),
        conjugation_selection: ConjugationSelection::Default,
    }
}

fn segment(candidate_id: i64, start: usize, end: usize, score: f64, entity: bool) -> Segment {
    Segment {
        candidate_id,
        start,
        end,
        score,
        common: None,
        entity,
        rules: None,
    }
}

fn group(start: usize, end: usize, matches: usize, segments: Vec<Segment>) -> PathPart {
    PathPart::Group(SegmentGroup {
        group_id: 1,
        start,
        end,
        segments,
        matches,
    })
}

fn fixture() -> Value {
    serde_json::from_str(include_str!("../../tests/fixtures/m3-projection.json")).unwrap()
}

#[test]
fn matches_typescript_proxy_tie_alternatives_components_counters_and_gaps() {
    let mut primary = candidate(CandidateKind::Proxy, "猫", "ねこ", 10);
    primary.components.push(CandidateComponent {
        text: u("猫"),
        true_text: None,
        route: Route::Kanji,
        reading: u("ねこ"),
        entry_index: Some(10),
        root: primary.root.clone(),
        inflection: Vec::new(),
        primary: true,
        public_seq: Some(10),
        physical_key: PhysicalKey::Sequence(10),
        physical_group: None,
        suffix_class: None,
        definition_seq: None,
        semantic_members: Vec::new(),
        identity_roots: Vec::new(),
        conjugation_selection: ConjugationSelection::Default,
    });
    primary.counter = Some(("Value: 1".to_owned(), false));
    let candidates = HashMap::from([
        (
            2,
            score_candidate(candidate(CandidateKind::Proxy, "猫", "ねこ", 20), &["n"]),
        ),
        (1, score_candidate(primary, &["pn"])),
        (
            3,
            score_candidate(candidate(CandidateKind::Simple, "猫", "びょう", 30), &["n"]),
        ),
    ]);
    let parts = vec![group(
        1,
        2,
        4,
        vec![
            segment(2, 1, 2, 100.0, false),
            segment(1, 1, 2, 100.0, false),
            segment(3, 1, 2, 60.0, false),
        ],
    )];
    let tokens = project_tokens(&u("X猫Y"), &parts, &candidates, &[]).unwrap();
    let actual = json!({
        "spans": tokens.iter().map(|token| [token.start, token.end]).collect::<Vec<_>>(),
        "candidateIds": tokens.iter().map(|token| token.candidate_id).collect::<Vec<_>>(),
        "primary": tokens[1],
    });
    assert_eq!(actual, fixture()["proxy"]);
}

#[test]
fn matches_typescript_contextual_nani_and_preapplied_annotation_hint() {
    let candidates = HashMap::from([
        (
            4,
            score_candidate(candidate(CandidateKind::Simple, "何", "なに", 4), &["pron"]),
        ),
        (
            5,
            score_candidate(
                candidate(CandidateKind::Simple, "では", "で\u{200c}は", 5),
                &["cop"],
            ),
        ),
    ]);
    let parts = vec![
        group(0, 1, 1, vec![segment(4, 0, 1, 10.0, false)]),
        group(1, 3, 1, vec![segment(5, 1, 3, 9.0, false)]),
    ];
    let tokens = project_tokens(&u("何では"), &parts, &candidates, &[]).unwrap();
    let actual = tokens
        .iter()
        .map(|token| {
            json!({
                "candidateId": token.candidate_id,
                "reading": token.reading,
                "romanized": token.romanized,
                "alternativeReading": token.alternatives[0].reading,
            })
        })
        .collect::<Vec<_>>();
    assert_eq!(json!(actual), fixture()["naniHint"]);
}

#[test]
fn matches_typescript_negative_entity_id_and_astral_utf16_span() {
    let parts = vec![group(0, 2, 1, vec![segment(-1, 0, 2, 50.0, true)])];
    let tokens = project_tokens(
        &u("😀"),
        &parts,
        &HashMap::new(),
        &[EntityHint {
            start: 0,
            end: 2,
            boost: None,
        }],
    )
    .unwrap();
    assert_eq!(
        serde_json::to_value(&tokens[0]).unwrap(),
        fixture()["entity"]
    );
}

#[test]
fn gap_and_shift_preserve_malformed_utf16_units_and_exact_offsets() {
    for units in [[0xd83d], [0xde00]] {
        let gap = gap(&units, 0, 1).unwrap();
        let shifted = shift_token(gap, 7).unwrap();
        assert_eq!((shifted.start, shifted.end), (7, 8));
        let serialized = serde_json::to_string(&shifted).unwrap();
        assert!(serialized.contains(if units[0] == 0xd83d {
            "\\ud83d"
        } else {
            "\\ude00"
        }));
        assert!(!serialized.contains('�'));
    }
}

#[test]
fn projects_path_score_and_rejects_inconsistent_spans_and_match_counts() {
    let path = PathResult {
        score: 2.5,
        parts: Vec::new(),
    };
    let projected = project_path(&u("猫"), &path, &HashMap::new(), &[]).unwrap();
    assert_eq!(projected.score, 2.5);
    assert_eq!(projected.tokens.len(), 1);

    let bad_span = vec![group(0, 2, 1, vec![segment(-1, 0, 2, 1.0, true)])];
    assert_eq!(
        project_tokens(&u("猫"), &bad_span, &HashMap::new(), &[])
            .unwrap_err()
            .code,
        ErrorCode::OutOfRange
    );

    let candidates = HashMap::from([(
        1,
        score_candidate(candidate(CandidateKind::Simple, "猫", "ねこ", 1), &[]),
    )]);
    let bad_matches = vec![group(0, 1, 0, vec![segment(1, 0, 1, 1.0, false)])];
    assert_eq!(
        project_tokens(&u("猫"), &bad_matches, &candidates, &[])
            .unwrap_err()
            .code,
        ErrorCode::Internal
    );
}
