use super::*;
use crate::analyzer_lexicon::{
    AnalysisInflection, AnalysisRoot, CandidateKind, ConjugationSelection, MaterializedCandidate,
    PhysicalKey, SemanticMember,
};
use crate::analyzer_model::{
    Conjugation, ConjugationProperty, EntryScoreFacts, ScoreCandidate, ScoreModifier,
    ScoreWordKind, SequenceFacts, WordScoreFacts,
};
use crate::morphology::Route;

fn u(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}

fn candidate(text: &str, seq: u32, kind: CandidateKind) -> MaterializedCandidate {
    let text = u(text);
    let root = AnalysisRoot {
        seq,
        form: text.clone(),
        reading: text.clone(),
    };
    let inflection = AnalysisInflection {
        pos: "v1".to_owned(),
        kind: 3,
        negative: Some(false),
        formal: None,
        ordinal: 0,
    };
    let conjugation = Conjugation {
        seq: i64::from(seq),
        from: i64::from(seq),
        via: None,
        property: ConjugationProperty {
            pos: inflection.pos.clone(),
            kind: inflection.kind,
            negative: inflection.negative,
            formal: inflection.formal,
        },
    };
    MaterializedCandidate {
        kind,
        text: text.clone(),
        true_text: text.clone(),
        route: Route::Kana,
        reading: text.clone(),
        public_seq: Some(seq),
        physical_seq: Some(i64::from(seq)),
        physical_key: PhysicalKey::Sequence(seq),
        physical_group: None,
        lookup_locators: Vec::new(),
        member_ord: Some(0),
        entry_index: None,
        root: Some(root.clone()),
        inflection: vec![inflection.clone()],
        score_facts: ScoreCandidate::Word(WordScoreFacts {
            kind: ScoreWordKind::Word,
            text: text.clone(),
            true_text: text,
            true_text_follows_text: true,
            route: Route::Kana,
            seq: Some(i64::from(seq)),
            ord: 0,
            common: None,
            nokanji: false,
            entry: Some(EntryScoreFacts {
                root: true,
                n_kanji: 0,
                primary_nokanji: false,
            }),
            conjugation_only: true,
            conjugations: vec![conjugation],
            positions: vec!["v1".to_owned()],
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
        definition_seq: Some(seq),
        semantic_members: vec![SemanticMember {
            entry_index: None,
            root: Some(root),
            inflection: vec![inflection],
            public_seq: Some(seq),
            physical_group: None,
            member_ord: Some(0),
            target_n_kanji: Some(0),
            target_n_kana: Some(1),
            via_seq: None,
            stage_groups: Vec::new(),
            stage_keys: Vec::new(),
            stage_member_ords: Vec::new(),
            stage_prop_ords: Vec::new(),
        }],
        identity_roots: vec![seq],
        conjugation_selection: ConjugationSelection::Default,
    }
}

#[test]
fn compound_preserves_component_order_scores_and_generated_identity() {
    let mut primary = candidate("食べて", 1_358_280, CandidateKind::Simple);
    primary.physical_seq = Some(-1_358_280);
    primary.physical_key = PhysicalKey::Semantic(crate::analyzer_lexicon::StageKey {
        root_seq: 1_358_280,
        aliases: vec![7, 9],
    });
    primary.reading = u("たべて");
    let mut suffix = candidate("いる", 1_577_980, CandidateKind::Simple);
    suffix.reading = u("いる");
    suffix.suffix_class = Some(":teiru".to_owned());
    let result = compound_suffix(
        &primary,
        &suffix,
        &u("いて"),
        &u("食べていて"),
        &SuffixCompound {
            modifier: ScoreModifier {
                multiplier: 3,
                constant: 0,
            },
            ..SuffixCompound::default()
        },
        Some(":teiru".to_owned()),
    );
    assert_eq!(result.kind, CandidateKind::Compound);
    assert_eq!(String::from_utf16_lossy(&result.reading), "たべていて");
    assert_eq!(result.physical_seq, Some(-1_358_280));
    assert_eq!(result.components.len(), 2);
    assert!(result.components[0].primary);
    assert!(!result.components[1].primary);
    assert_eq!(
        result.components[0].semantic_members,
        primary.semantic_members
    );
    let ScoreCandidate::Compound(facts) = result.score_facts else {
        panic!("expected compound score facts")
    };
    assert_eq!(facts.modifier.multiplier, 3);
    assert_eq!(facts.conjugations.len(), 1);
}

#[test]
fn nested_compounds_flatten_the_base_and_accumulate_modifiers() {
    let primary = candidate("食べ", 1, CandidateKind::Simple);
    let suffix = candidate("すぎる", 2, CandidateKind::Simple);
    let first = compound_suffix(
        &primary,
        &suffix,
        &u("すぎる"),
        &u("食べすぎる"),
        &SuffixCompound {
            modifier: ScoreModifier {
                multiplier: 5,
                constant: 20,
            },
            ..SuffixCompound::default()
        },
        Some(":sugiru".to_owned()),
    );
    let second = compound_suffix(
        &first,
        &candidate("たい", 3, CandidateKind::Simple),
        &u("たい"),
        &u("食べすぎたい"),
        &SuffixCompound {
            modifier: ScoreModifier {
                multiplier: 2,
                constant: 7,
            },
            ..SuffixCompound::default()
        },
        Some(":tai".to_owned()),
    );
    let ScoreCandidate::Compound(facts) = second.score_facts else {
        panic!("expected compound score facts")
    };
    assert_eq!(facts.modifier.multiplier, 7);
    assert_eq!(facts.modifier.constant, 27);
    assert_eq!(second.components.len(), 3);
}

#[test]
fn abbreviations_patch_utf16_readings_without_changing_true_text() {
    let mut source = candidate("しない", 1_157_170, CandidateKind::Simple);
    source.reading = u("😀しない");
    source.true_text = u("しない");
    let proxy = abbreviate_suffix(
        &source,
        &u("せ"),
        &u("ん"),
        &u("せん"),
        2,
        Some(&(u("しない"), u("せ"))),
    );
    assert_eq!(proxy.kind, CandidateKind::Proxy);
    assert_eq!(proxy.text, u("せん"));
    assert_eq!(proxy.true_text, u("しない"));
    assert_eq!(String::from_utf16_lossy(&proxy.reading), "😀せん");
    let ScoreCandidate::Word(facts) = proxy.score_facts else {
        panic!("expected word score facts")
    };
    assert!(!facts.true_text_follows_text);
}

#[test]
fn uniqueness_matches_the_frozen_suffix_rules() {
    let mut direct = candidate("さ", 1, CandidateKind::Simple);
    let ScoreCandidate::Word(facts) = &mut direct.score_facts else {
        unreachable!()
    };
    facts.conjugation_only = false;
    assert!(unique_suffix(":teba", &[]));
    assert!(unique_suffix(":sa", &[direct.clone()]));
    assert!(!unique_suffix(":desu", &[]));
    assert!(unique_suffix(":desu", &[direct]));
    assert!(!unique_suffix(":tai", &[]));
}
