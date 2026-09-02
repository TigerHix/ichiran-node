use std::fs;
use std::path::PathBuf;

use crate::morphology::Route;
use crate::pack::Pack;
use crate::support::{
    SupportSplit, SupportSplitConjugation, SupportSplitKind, SupportSplitPart, SupportSplitWord,
};

use super::{ANNOTATION_CACHE_BLOCKS, AnalyzerAnnotations};

fn release() -> PathBuf {
    std::env::var_os("ICHIRAN_M1_PACK_DIR")
        .map(PathBuf::from)
        .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory")
}

fn annotations() -> AnalyzerAnnotations {
    let pack = Pack::open(fs::read(release().join("hot.bin")).expect("read qualified hot.bin"))
        .expect("open qualified pack");
    AnalyzerAnnotations::open(pack.section_data(5).expect("read annotation section"))
        .expect("open annotation section")
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn real_pack_matches_typescript_annotation_witnesses_and_stays_lazy() {
    let mut reader = annotations();
    assert_eq!(reader.annotation_indexes.len(), 842);
    assert_eq!(reader.generated_indexes.len(), 37);
    assert!(reader.annotation_cache.is_empty());
    assert_eq!(reader.decoded_blocks, 0);

    let split = reader
        .split(1_008_450, Route::Kana, "では", SupportSplitKind::Segsplit)
        .unwrap()
        .expect("qualified split witness");
    assert_eq!(
        split,
        SupportSplit {
            definition_seq: 1_008_450,
            route: Route::Kana,
            surface: "では".to_owned(),
            kind: SupportSplitKind::Segsplit,
            parts: vec![
                SupportSplitPart::Word(SupportSplitWord {
                    seq: 2_028_980,
                    route: Route::Kana,
                    text: "で".to_owned(),
                    best: None,
                    ord: 0,
                    common: Some(0),
                    common_tags: "[spec1]".to_owned(),
                    conjugatable: true,
                    nokanji: false,
                    generated: Some(vec![SupportSplitConjugation {
                        from: 2_089_020,
                        via: false,
                        pos: "cop".to_owned(),
                        kind: 3,
                        negative: Some(false),
                        formal: Some(false),
                    }]),
                }),
                SupportSplitPart::Word(SupportSplitWord {
                    seq: 2_028_920,
                    route: Route::Kana,
                    text: "は".to_owned(),
                    best: None,
                    ord: 0,
                    common: Some(0),
                    common_tags: "[spec1]".to_owned(),
                    conjugatable: true,
                    nokanji: false,
                    generated: None,
                }),
            ],
            score: -5,
            primary: 0,
            connector: " ".to_owned(),
            root: Vec::new(),
        }
    );
    assert_eq!(reader.annotation_cache.len(), 1);
    assert_eq!(reader.decoded_blocks, 0);
    assert_eq!(
        reader
            .hint(1_008_450, Route::Kana, "では", "では")
            .unwrap()
            .as_deref(),
        Some("で\u{200c}は")
    );
    assert_eq!(reader.annotation_cache.len(), 1);

    assert_eq!(
        reader
            .lookup_order(Route::Kanji, "__ordinary__", 900_000, None)
            .unwrap(),
        Some(0)
    );
    let after_direct = reader.decoded_blocks;
    assert!(after_direct > 0 && after_direct < reader.generated_indexes.len());
    assert_eq!(
        reader
            .lookup_order(Route::Kanji, "__ordinary__", 1_000_300, Some(&[768]),)
            .unwrap(),
        Some(0)
    );
    assert_eq!(
        reader
            .lookup_order(Route::Kana, "あえなく", 1_212_870, None)
            .unwrap(),
        Some(1)
    );
    let before_owned_miss = reader.decoded_blocks;
    assert_eq!(
        reader
            .lookup_order(Route::Kana, "あえなく", u32::MAX - 1, None)
            .unwrap(),
        None
    );
    assert_eq!(reader.decoded_blocks, before_owned_miss);

    reader.preload_all_generated().unwrap();
    assert_eq!(reader.decoded_blocks, 37);
    assert_eq!(reader.decoded_records, 764_828);
    assert_eq!(reader.decoded_order_roots, 9_635);
    assert_eq!(reader.decoded_order_max_rank, 38);
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn ordinary_annotation_cache_is_a_promoting_sixteen_block_lru() {
    let mut reader = annotations();
    let seqs = reader
        .annotation_indexes
        .iter()
        .take(ANNOTATION_CACHE_BLOCKS + 1)
        .map(|index| index.seq)
        .collect::<Vec<_>>();
    for seq in &seqs[..ANNOTATION_CACHE_BLOCKS] {
        assert!(reader.load_annotation(*seq).unwrap());
    }
    assert_eq!(reader.annotation_cache.len(), ANNOTATION_CACHE_BLOCKS);
    assert!(reader.load_annotation(seqs[0]).unwrap());
    assert!(
        reader
            .load_annotation(seqs[ANNOTATION_CACHE_BLOCKS])
            .unwrap()
    );
    assert_eq!(reader.annotation_cache.len(), ANNOTATION_CACHE_BLOCKS);
    assert!(reader.annotation_cache.contains_key(&seqs[0]));
    assert!(!reader.annotation_cache.contains_key(&seqs[1]));
    assert!(
        reader
            .annotation_cache
            .contains_key(&seqs[ANNOTATION_CACHE_BLOCKS])
    );
    assert_eq!(reader.decoded_blocks, 0);
}
