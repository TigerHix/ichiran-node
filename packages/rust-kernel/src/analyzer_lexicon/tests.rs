use std::fs;
use std::path::PathBuf;

use super::*;
use crate::analyzer_model::ScoreCandidate;
use crate::annotations::AnalyzerAnnotations;
use crate::morphology::Morphology;
use crate::pack::Pack;

fn release() -> PathBuf {
    std::env::var_os("ICHIRAN_M1_PACK_DIR")
        .map(PathBuf::from)
        .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory")
}

fn key(value: &PhysicalKey) -> String {
    match value {
        PhysicalKey::Sequence(seq) => format!("seq:{seq}"),
        PhysicalKey::Semantic(stage) => format!(
            "semantic:{}:{}",
            stage.root_seq,
            stage
                .aliases
                .iter()
                .map(u16::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ),
        PhysicalKey::Counter(text) => format!("counter:{}", utf16_string(text, "counter").unwrap()),
    }
}

fn stage_key(value: &Option<StageKey>) -> Option<String> {
    value.as_ref().map(|stage| {
        format!(
            "{}:{}",
            stage.root_seq,
            stage
                .aliases
                .iter()
                .map(u16::to_string)
                .collect::<Vec<_>>()
                .join(",")
        )
    })
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn typescript_lexicon_materialization_fixture_is_exact() {
    let pack = Pack::open(fs::read(release().join("hot.bin")).expect("read qualified hot.bin"))
        .expect("open qualified pack");
    let surface = SurfaceIndex::open(pack.section_data(1).unwrap()).unwrap();
    let roots = RootPayload::open(pack.section_data(2).unwrap()).unwrap();
    let morphology = Morphology::open(pack.section_data(3).unwrap()).unwrap();
    let support = AnalyzerSupport::open(pack.section_data(4).unwrap()).unwrap();
    let mut annotations = AnalyzerAnnotations::open(pack.section_data(5).unwrap()).unwrap();
    let mut lexicon =
        AnalyzerLexicon::new(&surface, &roots, &morphology, &support, &mut annotations);

    let direct = lexicon.lexical(&utf16("猫")).unwrap();
    assert_eq!(direct.len(), 2);
    assert_eq!(
        direct
            .iter()
            .map(|value| (
                value.public_seq,
                value.physical_seq,
                key(&value.physical_key),
                utf16_string(&value.reading, "reading").unwrap()
            ))
            .collect::<Vec<_>>(),
        vec![
            (
                Some(1_467_640),
                Some(1_467_640),
                "seq:1467640".into(),
                "ねこ".into()
            ),
            (
                Some(2_698_030),
                Some(2_698_030),
                "seq:2698030".into(),
                "ねこま".into()
            ),
        ]
    );
    let ScoreCandidate::Word(cat) = &direct[0].score_facts else {
        panic!("direct cat is not a word")
    };
    assert_eq!((cat.common, cat.ord), (Some(7), 0));
    assert_eq!(cat.positions, ["n"]);

    let past = lexicon.lexical(&utf16("食べた")).unwrap();
    assert_eq!(past.len(), 1);
    let past = &past[0];
    assert_eq!(
        (
            past.public_seq,
            past.physical_seq,
            key(&past.physical_key),
            past.physical_group
        ),
        (
            Some(1_358_280),
            Some(-1_358_280),
            "semantic:1358280:85".into(),
            None
        )
    );
    assert_eq!(past.inflection.len(), 1);
    assert_eq!(
        (
            &past.inflection[0].pos,
            past.inflection[0].kind,
            past.inflection[0].negative,
            past.inflection[0].formal,
            past.inflection[0].ordinal
        ),
        (&"v1".to_owned(), 2, Some(false), Some(false), 1)
    );

    let generated = lexicon.lexical(&utf16("忘れた")).unwrap();
    assert_eq!(generated.len(), 1);
    let generated = &generated[0];
    assert_eq!(generated.physical_group, Some(52_633));
    assert_eq!(generated.member_ord, Some(0));
    assert_eq!(generated.identity_roots, [1_519_210, 1_519_190]);
    assert_eq!(
        generated
            .lookup_locators
            .iter()
            .map(|locator| (locator.root_seq, locator.aliases.clone()))
            .collect::<Vec<_>>(),
        vec![
            (1_519_190, Some(vec![581, 85])),
            (1_519_210, Some(vec![85])),
        ]
    );
    assert_eq!(generated.semantic_members.len(), 2);
    let secondary = &generated.semantic_members[1];
    assert_eq!(
        secondary.root.as_ref().map(|root| root.seq),
        Some(1_519_190)
    );
    assert_eq!(secondary.member_ord, Some(1));
    assert_eq!(secondary.via_seq, Some(1_519_210));
    assert_eq!(secondary.stage_groups, [None, Some(52_633)]);
    assert_eq!(
        secondary
            .stage_keys
            .iter()
            .map(stage_key)
            .collect::<Vec<_>>(),
        [Some("1519190:581".into()), Some("1519190:581,85".into())]
    );
    assert_eq!(secondary.stage_member_ords, [Some(0), Some(1)]);
    assert_eq!(secondary.stage_prop_ords, [None, Some(0)]);

    let collision = lexicon.lexical(&utf16("あげつらい")).unwrap();
    assert_eq!(collision.len(), 1);
    let collision = &collision[0];
    assert_eq!(
        (
            collision.public_seq,
            collision.physical_seq,
            key(&collision.physical_key)
        ),
        (Some(2_735_620), Some(2_735_620), "seq:2735620".into())
    );
    assert_eq!(collision.identity_roots, [2_735_620, 1_000_280]);
    assert_eq!(collision.semantic_members.len(), 2);
    assert_eq!(
        collision.semantic_members[1]
            .root
            .as_ref()
            .map(|root| root.seq),
        Some(1_000_280)
    );
    assert_eq!(collision.semantic_members[1].inflection[0].kind, 13);

    let ordered = lexicon.lexical(&utf16("いた")).unwrap();
    assert_eq!(
        ordered
            .iter()
            .map(|value| value.public_seq.unwrap())
            .collect::<Vec<_>>(),
        [
            1_432_680, 1_577_980, 2_851_105, 1_322_180, 2_729_170, 1_587_780, 2_851_106, 2_248_980,
            1_481_350,
        ]
    );
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn typescript_suffix_recursion_and_segment_split_fixtures_are_exact() {
    let pack = Pack::open(fs::read(release().join("hot.bin")).expect("read qualified hot.bin"))
        .expect("open qualified pack");
    let surface = SurfaceIndex::open(pack.section_data(1).unwrap()).unwrap();
    let roots = RootPayload::open(pack.section_data(2).unwrap()).unwrap();
    let morphology = Morphology::open(pack.section_data(3).unwrap()).unwrap();
    let support = AnalyzerSupport::open(pack.section_data(4).unwrap()).unwrap();
    let mut annotations = AnalyzerAnnotations::open(pack.section_data(5).unwrap()).unwrap();
    let mut lexicon =
        AnalyzerLexicon::new(&surface, &roots, &morphology, &support, &mut annotations);

    let desire = lexicon.full(&utf16("食べたい")).unwrap();
    assert_eq!(desire.len(), 1);
    assert_eq!(desire[0].kind, CandidateKind::Compound);
    assert_eq!(desire[0].public_seq, Some(1_358_280));
    assert_eq!(desire[0].physical_seq, Some(-1_358_280));
    assert_eq!(desire[0].suffix_class.as_deref(), Some(":tai"));
    assert_eq!(
        utf16_string(&desire[0].reading, "desire reading").unwrap(),
        "たべたい"
    );
    assert_eq!(
        desire[0]
            .components
            .iter()
            .map(|value| (
                utf16_string(&value.text, "desire component").unwrap(),
                value.public_seq,
                value.suffix_class.clone(),
                value.conjugation_selection,
            ))
            .collect::<Vec<_>>(),
        vec![
            (
                "食べ".into(),
                Some(1_358_280),
                None,
                ConjugationSelection::Explicit,
            ),
            (
                "たい".into(),
                Some(2_017_560),
                Some(":tai".into()),
                ConjugationSelection::Root,
            ),
        ]
    );

    // `:nakereba` restores 食べなければ through `full_at`, then abbreviates it.
    let recursive = lexicon.full(&utf16("食べなきゃ")).unwrap();
    assert_eq!(recursive.len(), 1);
    assert_eq!(recursive[0].kind, CandidateKind::Proxy);
    assert_eq!(recursive[0].public_seq, Some(1_358_280));
    assert_eq!(recursive[0].physical_seq, Some(-1_358_280));
    assert_eq!(
        utf16_string(&recursive[0].reading, "recursive reading").unwrap(),
        "たべなきゃ"
    );

    // The seekable annotations merge both generated 忘れる identities in
    // stable primary/secondary order, matching the TypeScript Worker path.
    let generated = lexicon.full(&utf16("忘れたそう")).unwrap();
    assert_eq!(generated.len(), 1);
    assert_eq!(generated[0].public_seq, Some(1_519_210));
    assert_eq!(generated[0].physical_seq, Some(-1_519_210));
    assert_eq!(generated[0].physical_group, Some(52_620));
    assert_eq!(generated[0].suffix_class.as_deref(), Some(":tasou"));
    assert_eq!(
        generated[0]
            .semantic_members
            .iter()
            .map(|member| member.public_seq)
            .collect::<Vec<_>>(),
        vec![Some(1_519_210), Some(1_519_190)]
    );

    let dewa = lexicon
        .lexical(&utf16("では"))
        .unwrap()
        .into_iter()
        .find(|value| value.definition_seq == Some(1_008_450))
        .expect("qualified では split source");
    let split = lexicon
        .segment_split(&dewa)
        .unwrap()
        .expect("qualified では segment split");
    assert_eq!(split.added_score, -5);
    assert_eq!(split.candidate.kind, CandidateKind::Compound);
    assert_eq!(
        split
            .candidate
            .components
            .iter()
            .map(|value| (value.public_seq, value.primary))
            .collect::<Vec<_>>(),
        vec![(Some(2_028_980), true), (Some(2_028_920), false)]
    );
    assert_eq!(
        utf16_string(&split.candidate.text, "split text").unwrap(),
        "では"
    );
    assert_eq!(
        utf16_string(&split.candidate.reading, "split reading").unwrap(),
        "で \u{200c}は"
    );
}
