use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use crate::binary::ByteSlice;
use crate::error::ErrorCode;
use crate::morphology::Route;
use crate::pack::Pack;

use super::{
    AnalyzerSupport, SupportCollision, SupportConjugation, SupportConjugationProperty,
    SupportConjugations, SupportCounterClass, SupportCounterSource, SupportDigit,
    SupportDigitOption, SupportSplitKind, SupportSplitPart, SupportSplitWord, SupportStats,
    SupportSuffixForm, SupportSuffixValue,
};

// Generated once by the authoritative TypeScript `buildAnalyzerSupport` from the
// exact source object in packages/core/tests/analyzer-support.test.ts.
const TYPESCRIPT_FIXTURE: &str = "49414e53555030310200e000000400008e9c8e8015a3310f0200000003000000010000000100000002000000020000000200000003000000050000000600000002000000030000000100000001000000190000007b000000e0000000f80000001001000030010000480100005801000070010000f001000014020000280200004002000088020000dc020000f0020000140300007c0300000300000003000000f80300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000c00000000000000010000000e0000000100000002000000050000000000000007000000ffffffff02000000ffffffffa0bb0d000d000000ffffffff000000000000000000000100ff02000000000000c1c62d0018c91e00ffffffff090000000d00080000000000a0bb0d000600000078f51e00030000001700000000000000010000001800000001000000010000001700000013000000ffffffff2236160017000000010000000000000000000000020000000000000000000000000000000000000100ff00000000000000000000180000001300000014000000223616001700000002000000010000000200000001000000000000000200000002000000030000010100000000000000000000000300010000000000000000000a0000000100000000000000030001000100000000000000040000000400000008000000090000000a0000000100000002000000010000000200000003000000010000007b000000150000000000000014000000000000000500000001000100000100000000000042630f001100000001000000fbffffffffffffff060000000200000000000100000000000100000000000000000000000000000000000000000000000000000000000200b4f51e000f000000ffffffff0b00000000000000000000000000020078f51e0012000000ffffffff0b000000000000000000000042630f001100000011000000100000000000000044430f00ba450f001600000003000000ffffffff010001000300000002000d04ffffffff00000000000000000100000006000000090000000b0000000f000000150000001a00000023000000280000002b00000030000000330000003c00000042000000450000004e00000054000000570000005d000000600000006300000072000000750000007b000000203a636861753a68613a723a7461693a7461736f753a746562615b6f7264696e616c5d61646a2d696578706963686931e3819fe3819fe3819de38186e381a1e38283e381a7e381a7e2808ce381afe381a7e381afe381afe381bbe38293e38281e4be8be682aae381a9e3818be381a3e3819fe69cace69cace79bae000200000001000000";

fn fixture() -> Vec<u8> {
    TYPESCRIPT_FIXTURE
        .as_bytes()
        .chunks_exact(2)
        .map(|pair| {
            let text = std::str::from_utf8(pair).unwrap();
            u8::from_str_radix(text, 16).unwrap()
        })
        .collect()
}

fn open(bytes: Vec<u8>) -> crate::Result<AnalyzerSupport> {
    let bytes = Arc::new(bytes);
    let section = ByteSlice::new(Arc::clone(&bytes), 0, bytes.len())?;
    AnalyzerSupport::open(section)
}

fn utf16(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}

#[test]
fn decodes_the_authoritative_typescript_fixture_exactly() {
    let support = open(fixture()).unwrap();
    let stats = support.stats();
    assert_eq!(
        stats,
        SupportStats {
            byte_length: 1024,
            suffix_keys: 2,
            suffix_values: 3,
            suffix_forms: 1,
            suffix_conjugations: 1,
            suffix_classes: 2,
            counter_keys: 2,
            counter_variants: 2,
            digit_options: 3,
            list_members: 5,
            number_members: 6,
            splits: 2,
            split_parts: 3,
            hints: 1,
            collisions: 1,
            generated_rules: 3,
            generated_aliases: 3,
            strings: 25,
            string_bytes: 123,
        }
    );
    assert_eq!(support.strings.borrow().iter().flatten().count(), 0);

    assert_eq!(
        support.suffix(&utf16("た")).unwrap(),
        vec![SupportSuffixValue {
            keyword: ":tai".into(),
            form: Some(SupportSuffixForm {
                seq: 900_000,
                text: "たそう".into(),
                best_kanji: None,
                common_tags: "".into(),
                ord: 0,
                common: None,
                conjugatable: false,
                nokanji: true,
                conjugations: Some(SupportConjugations::Values(vec![SupportConjugation {
                    seq: 3_000_001,
                    from: 2_017_560,
                    via: None,
                    property: SupportConjugationProperty {
                        pos: "adj-i".into(),
                        kind: 13,
                        negative: Some(false),
                        formal: None,
                    },
                }])),
            }),
        }]
    );
    assert_eq!(
        support.suffix(&utf16("ちゃ")).unwrap(),
        vec![
            SupportSuffixValue {
                keyword: ":teba".into(),
                form: None,
            },
            SupportSuffixValue {
                keyword: ":chau".into(),
                form: None,
            },
        ]
    );
    assert!(support.suffix(&utf16("missing")).unwrap().is_empty());
    let suffix_matches = support
        .suffix_matches_ending_at(&utf16("xたちゃ"), 4, 50)
        .unwrap();
    assert_eq!(suffix_matches.len(), 1);
    assert_eq!(
        (
            suffix_matches[0].start,
            suffix_matches[0].end,
            suffix_matches[0].text.as_str()
        ),
        (2, 4, "ちゃ")
    );
    assert!(
        support
            .suffix_matches_ending_at(&utf16("xたちゃ"), 4, 1)
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        support.suffix_class(900_000).unwrap().as_deref(),
        Some(":tasou")
    );
    assert_eq!(support.suffix_class(1).unwrap(), None);

    let counter = support.counters(&utf16("本目")).unwrap();
    assert_eq!(counter.len(), 1);
    assert_eq!(counter[0].class_name, SupportCounterClass::CounterText);
    assert_eq!(counter[0].text, "本目");
    assert_eq!(counter[0].kana, "ほん");
    assert_eq!(counter[0].suffix.as_deref(), Some("め"));
    assert_eq!(
        counter[0].source,
        Some(SupportCounterSource {
            seq: 1_455_650,
            route: Route::Kanji,
            text: "本".into(),
            ord: 0,
        })
    );
    assert!(counter[0].ordinal);
    assert_eq!(counter[0].common, Some(0));
    assert_eq!(counter[0].suffix_descriptions, ["[ordinal]"]);
    assert_eq!(
        counter[0].digit_options,
        [SupportDigitOption {
            digit: SupportDigit::Digit(3),
            values: vec![":r".into()],
        }]
    );
    assert_eq!(counter[0].digit_set, [1, 2]);
    assert_eq!(counter[0].allowed, [1, 2, 3]);
    let counter_matches = support
        .counter_matches_starting_at(&utf16("3本目先"), 1, 50)
        .unwrap();
    assert_eq!(
        counter_matches
            .iter()
            .map(|value| (value.start, value.end, value.text.as_str()))
            .collect::<Vec<_>>(),
        [(1, 3, "本目"), (1, 2, "本")]
    );

    let split = support
        .split(
            1_008_450,
            Route::Kana,
            &utf16("では"),
            SupportSplitKind::Segsplit,
        )
        .unwrap()
        .unwrap();
    assert_eq!(split.score, -5);
    assert_eq!(split.connector, " ");
    assert_eq!(split.parts.len(), 2);
    assert_eq!(
        split.parts[0],
        SupportSplitPart::Word(SupportSplitWord {
            seq: 2_028_980,
            route: Route::Kana,
            text: "で".into(),
            best: None,
            ord: 0,
            common: Some(0),
            common_tags: "ichi1".into(),
            conjugatable: false,
            nokanji: true,
            generated: None,
        })
    );
    assert_eq!(
        support
            .split(123, Route::Kanji, &utf16("例"), SupportSplitKind::Split)
            .unwrap()
            .unwrap()
            .parts,
        [SupportSplitPart::Score]
    );
    assert_eq!(
        support
            .hint(1_008_450, Route::Kana, &utf16("では"), &utf16("では"))
            .unwrap()
            .as_deref(),
        Some("で\u{200c}は")
    );
    assert_eq!(
        support
            .hint(1_008_450, Route::Kana, &utf16("では"), &utf16("でわ"))
            .unwrap(),
        None
    );
    assert_eq!(
        support
            .collision(1_000_260, Route::Kanji, &utf16("悪どかった"), &[3])
            .unwrap(),
        Some(SupportCollision {
            root_seq: 1_000_260,
            collision_seq: 1_000_890,
            via_seq: None,
            route: Route::Kanji,
            surface: "悪どかった".into(),
            rule_ids: vec![3],
            n_kanji: 1,
            n_kana: 1,
            primary_nokanji: false,
            archived: true,
            prefer_kana: true,
            prefer_kana_on_ordinal_zero: false,
            pos: vec!["adj-i".into(), "exp".into()],
            skip_word: false,
            final_particle: false,
            semi_final_particle: false,
            non_final_particle: false,
            copula: false,
            no_kanji_break_penalty: true,
        })
    );
    assert_eq!(support.generated_aliases(&[0]).unwrap(), [2]);
    assert_eq!(support.generated_aliases(&[1, 2]).unwrap(), [0, 1]);
    assert!(support.strings.borrow().iter().flatten().count() > 0);
}

#[test]
fn utf16_match_offsets_and_malformed_surrogates_are_exact() {
    let support = open(fixture()).unwrap();
    let astral_prefix = [0xd83d, 0xde00, 0x672c, 0x76ee];
    let matches = support
        .counter_matches_starting_at(&astral_prefix, 2, 50)
        .unwrap();
    assert_eq!(
        matches
            .iter()
            .map(|value| (value.start, value.end, value.text.as_str()))
            .collect::<Vec<_>>(),
        [(2, 4, "本目"), (2, 3, "本")]
    );
    for malformed in [[0xd83d, 0x672c], [0xde00, 0x672c]] {
        assert!(support.counters(&malformed).unwrap().is_empty());
        assert!(support.suffix(&malformed).unwrap().is_empty());
    }
    let error = support
        .counter_matches_starting_at(&utf16("本"), 2, 50)
        .unwrap_err();
    assert_eq!(error.code, ErrorCode::OutOfRange);
}

#[test]
fn strict_validation_rejects_rechecksummed_noncanonical_payloads() {
    let original = fixture();
    let cases = [
        (234, 1_u8, "suffix-key reserved byte"),
        (420, 0x80, "counter flag"),
        (607, 1, "split reserved byte"),
    ];
    for (offset, value, label) in cases {
        let mut bytes = original.clone();
        bytes[offset] = value;
        rechecksum(&mut bytes);
        let error = open(bytes)
            .err()
            .unwrap_or_else(|| panic!("accepted {label}"));
        assert_eq!(error.code, ErrorCode::CorruptPayload, "{label}: {error}");
    }
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn qualified_pack_matches_typescript_support_witnesses() {
    let directory = std::env::var_os("ICHIRAN_M1_PACK_DIR")
        .map(PathBuf::from)
        .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory");
    let pack = Pack::open(fs::read(directory.join("hot.bin")).unwrap()).unwrap();
    let support = AnalyzerSupport::open(pack.section_data(4).unwrap()).unwrap();
    let stats = support.stats();
    assert_eq!(
        stats,
        SupportStats {
            byte_length: 949_424,
            suffix_keys: 5_532,
            suffix_values: 5_533,
            suffix_forms: 5_193,
            suffix_conjugations: 5_225,
            suffix_classes: 3_586,
            counter_keys: 760,
            counter_variants: 799,
            digit_options: 244,
            list_members: 8_339,
            number_members: 351,
            splits: 0,
            split_parts: 0,
            hints: 0,
            collisions: 5_442,
            generated_rules: 1_161,
            generated_aliases: 1_030,
            strings: 11_467,
            string_bytes: 176_226,
        }
    );
    let suffix = support.suffix(&utf16("た")).unwrap();
    assert_eq!(suffix.len(), 1);
    assert_eq!(suffix[0].keyword, ":teiru");
    let form = suffix[0].form.as_ref().unwrap();
    assert_eq!(
        (form.seq, form.text.as_str(), form.nokanji),
        (10_551_837, "いた", false)
    );
    assert_eq!(
        support.suffix_class(900_000).unwrap().as_deref(),
        Some(":tasou")
    );

    let counters = support.counters(&utf16("本目")).unwrap();
    assert_eq!(counters.len(), 2);
    assert_eq!(counters[0].class_name, SupportCounterClass::CounterHifumi);
    assert_eq!(counters[0].source.as_ref().unwrap().seq, 1_260_670);
    assert_eq!(counters[1].class_name, SupportCounterClass::CounterText);
    assert_eq!(counters[1].source.as_ref().unwrap().seq, 1_522_150);
    assert!(counters.iter().all(|value| value.ordinal));
    let matches = support
        .counter_matches_starting_at(&utf16("3本目先"), 1, 50)
        .unwrap();
    assert_eq!(
        matches
            .iter()
            .map(|value| (
                value.start,
                value.end,
                value.text.as_str(),
                value.values.len()
            ))
            .collect::<Vec<_>>(),
        [(1, 3, "本目", 2), (1, 2, "本", 2), (1, 1, "", 1)]
    );
    assert_eq!(support.generated_aliases(&[0]).unwrap(), [0]);
    assert_eq!(support.generated_aliases(&[1, 2]).unwrap(), [1, 2]);
    let collision = support
        .collision(1_000_280, Route::Kana, &utf16("あげつらい"), &[885])
        .unwrap()
        .unwrap();
    assert_eq!(collision.collision_seq, 2_735_620);
    assert_eq!(collision.pos, ["n"]);
}

fn rechecksum(bytes: &mut [u8]) {
    let payload = crc32fast::hash(&bytes[224..]);
    bytes[20..24].copy_from_slice(&payload.to_le_bytes());
    bytes[16..20].fill(0);
    let header = crc32fast::hash(&bytes[..224]);
    bytes[16..20].copy_from_slice(&header.to_le_bytes());
}
