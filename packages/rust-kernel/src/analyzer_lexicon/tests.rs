use std::cell::RefCell;
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
        PhysicalKey::Unique(index) => format!("unique:{index}"),
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
    let split_calls = RefCell::new(Vec::new());
    let score_split = |seq, route, text: &[u16]| {
        split_calls.borrow_mut().push((seq, route, text.to_vec()));
        Ok(None)
    };
    let mut lexicon = AnalyzerLexicon::new(
        &surface,
        &roots,
        &morphology,
        &support,
        &mut annotations,
        &score_split,
    );

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

    let calls = split_calls.borrow();
    assert!(calls.iter().any(|(seq, _, _)| *seq == 2_735_620));
    assert!(calls.iter().any(|(seq, _, _)| *seq == 1_000_280));
}
