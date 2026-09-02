use ichiran_kernel::characters::{
    self, BasicSplitSegment, BasicSplitType, CharClass, KANA_CHARACTERS,
};
use ichiran_kernel::numbers::{self, NumberKana};
use ichiran_kernel::romanization::{self, RomanizationName};

fn u(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}
fn s(value: &[u16]) -> String {
    String::from_utf16(value).unwrap()
}

#[test]
fn character_literal_ngram_differential_fixtures() {
    assert_eq!(
        characters::simplify_ngrams(&u("a.b.a"), &[(u("."), u("$&$&"))]),
        u("a..b..a")
    );
    assert_eq!(
        characters::simplify_ngrams(&u("aaaa"), &[(u("aa"), u("b"))]),
        u("bb")
    );
    assert_eq!(
        characters::simplify_ngrams(&u("A😀B"), &[(vec![], u("-"))]),
        vec![0x2d, 0x41, 0x2d, 0xd83d, 0xde00, 0x2d, 0x42, 0x2d],
    );
}

#[test]
fn character_normalization_and_split_differential_fixtures() {
    let cases = [
        ("食べました。", "食べました. "),
        ("東京２０２６年８月２８日", "東京2026年8月28日"),
        ("ﾊﾟﾝを二つ、ｺｰﾋｰを１杯。", "パンを二つ, コーヒーを1杯. "),
        ("「何で？」と言った・・・", " \"何で?\" と言った... "),
        ("す゛っと", "す\"っと"),
    ];
    for (input, expected) in cases {
        assert_eq!(s(&characters::normalize(&u(input), false, false)), expected);
    }
    assert_eq!(
        s(&characters::normalize(&u("す゛っと"), false, true)),
        "ずっと"
    );
    assert_eq!(
        s(&characters::normalize(&u("ﾊﾟﾝを１杯。"), true, false)),
        "パンを１杯。"
    );

    assert_eq!(
        characters::basic_split(&u("abc, 12.34 / 日本語")),
        vec![
            BasicSplitSegment {
                kind: BasicSplitType::Misc,
                text: u("abc, 12.34 / ")
            },
            BasicSplitSegment {
                kind: BasicSplitType::Word,
                text: u("日本語")
            },
        ],
    );
}

#[test]
fn character_classification_conversion_and_measurement_fixtures() {
    assert_eq!(characters::get_char_class(&u("か")), u("ka"));
    assert_eq!(characters::get_char_class(&[0xd83d]), vec![0xd83d]);
    assert_eq!(characters::voice_char("shi"), "ji");
    assert!(characters::long_vowel_modifier_p("+o", &u("こ")));
    assert!(!characters::long_vowel_modifier_p("+a", &u("こ")));
    assert!(characters::test_word(&u("日本語"), CharClass::Kanji));
    assert_eq!(
        characters::count_char_class(&u("a日本12語z"), CharClass::Kanji),
        3
    );
    assert_eq!(
        characters::collect_char_class(&u("a日本"), CharClass::Kanji),
        vec![u("日"), u("本")]
    );
    assert_eq!(
        characters::consecutive_char_groups(CharClass::Kanji, &u("a日本12語z"), 0, 7),
        vec![(1, 3), (5, 6)]
    );
    assert_eq!(
        characters::sequential_kanji_positions(&u("東京２０２６年８月２８日"), 3),
        vec![4]
    );
    assert_eq!(s(&characters::as_hiragana(&u("ﾊﾟﾝを二つ"))), "はﾟんを二つ");
    assert_eq!(
        s(&characters::as_katakana(&u("右に出る者はいない"))),
        "右ニ出ル者ハイナイ"
    );
    assert_eq!(
        characters::mora_length(&u("ヴァイオリンとゔぁいおりん")),
        11
    );
    assert_eq!(characters::mora_length(&u("😀")), 1);
    assert_eq!(
        characters::mora_length(&[0xd83d, 0xde00, 0xde00, 0xd83d]),
        3
    );
}

#[test]
fn character_kana_mutation_and_stemming_fixtures() {
    for (_, kana) in KANA_CHARACTERS {
        for unit in kana.encode_utf16() {
            let input = vec![unit, 0x306a];
            assert_eq!(
                characters::unrendaku(&characters::rendaku(&input, false)),
                characters::unrendaku(&input)
            );
            assert_eq!(characters::geminate(&input), vec![unit, 0x3063]);
        }
    }
    assert_eq!(s(&characters::rendaku(&u("さん"), false)), "ざん");
    assert_eq!(s(&characters::rendaku(&u("ひゃく"), true)), "ぴゃく");
    assert_eq!(s(&characters::unrendaku(&u("ぴゃく"))), "ひゃく");
    assert_eq!(
        s(&characters::destem(&u("食べました。"), 2, CharClass::Kana)),
        "食べま"
    );
    assert_eq!(
        characters::destem(&u("食"), 2, CharClass::Kana),
        Vec::<u16>::new()
    );
}

#[test]
fn japanese_number_generation_differential_fixtures() {
    let cases = [
        (0.0, "〇", "れい"),
        (10.0, "十", "じゅう"),
        (300.0, "三百", "さんびゃく"),
        (600.0, "六百", "ろっぴゃく"),
        (800.0, "八百", "はっぴゃく"),
        (3000.0, "三千", "さんぜん"),
        (8000.0, "八千", "はっせん"),
        (
            12345.0,
            "一万二千三百四十五",
            "いちまん にせん さんびゃく よんじゅう ご",
        ),
        (100000001.0, "一億一", "いちおく いち"),
        (
            9876543210.0,
            "九十八億七千六百五十四万三千二百十",
            "きゅうじゅう はちおく ななせん ろっぴゃく ごじゅう よんまん さんぜん にひゃく じゅう",
        ),
        (100000000000000.0, "百兆", "ひゃくちょう"),
    ];
    for (number, kanji, kana) in cases {
        assert_eq!(s(&numbers::number_to_kanji(number).unwrap()), kanji);
        assert_eq!(s(&numbers::number_to_kana(number).unwrap()), kana);
    }
}

#[test]
fn japanese_number_parse_differential_fixtures() {
    for (input, expected) in [
        ("０", 0.0),
        ("123", 123.0),
        ("一二三", 123.0),
        ("千二百三十四", 1234.0),
        ("壱拾参", 13.0),
        ("一億二万三", 100020003.0),
        ("", 0.0),
    ] {
        assert_eq!(numbers::parse_number(&u(input)).unwrap(), expected);
    }
}

#[test]
fn japanese_number_error_and_group_differential_fixtures() {
    assert_eq!(
        numbers::number_to_kanji(-1.0).unwrap_err().to_string(),
        "Number must be a non-negative integer"
    );
    assert_eq!(
        numbers::number_to_kanji(1.5).unwrap_err().to_string(),
        "Number must be a non-negative integer"
    );
    let error = numbers::parse_number(&u("一a")).unwrap_err();
    assert_eq!(
        s(&error.message_utf16()),
        "\"一a\" is not a number: Invalid character: a"
    );
    assert_eq!(
        numbers::number_to_kana_with_separator(12345.0, None).unwrap(),
        NumberKana::Groups(vec![
            u("いちまん"),
            u("にせん"),
            u("さんびゃく"),
            u("よんじゅう"),
            u("ご")
        ]),
    );
    assert_eq!(
        numbers::number_to_kana_from_kanji(&u("123"), Some(&u(" "))),
        NumberKana::Joined(u("  ")),
    );
}

#[test]
fn romanization_all_methods_differential_fixtures() {
    let words = [
        "きゃ",
        "しゃ",
        "ちゃ",
        "じゃ",
        "ぢゃ",
        "ふぁ",
        "うぉ",
        "がっこう",
        "まっちゃ",
        "しんぶん",
        "しんよう",
        "スーパー",
        "おおさか",
        "とうきょう",
        "おねえさん",
        "ヴァイオリン",
        "時々",
        "いすゞ",
        "は\u{200c}は",
        "へ\u{200c}へ",
        "aかな12",
    ];
    let fixtures: [(RomanizationName, [&str; 21]); 6] = [
        (
            RomanizationName::HepburnBasic,
            [
                "kya", "sha", "cha", "ja", "ja", "fa", "wo", "gakkou", "matcha", "shinbun",
                "shin'you", "supa", "oosaka", "toukyou", "oneesan", "vaiorin", "時々", "isuzu",
                "hawa", "hee", "akana12",
            ],
        ),
        (
            RomanizationName::HepburnSimple,
            [
                "kya", "sha", "cha", "ja", "ja", "fa", "wo", "gakko", "matcha", "shinbun",
                "shin'yo", "supa", "osaka", "tokyo", "oneesan", "vaiorin", "時々", "isuzu", "hawa",
                "hee", "akana12",
            ],
        ),
        (
            RomanizationName::HepburnPassport,
            [
                "kya", "sha", "cha", "ja", "ja", "fa", "wo", "gakkoh", "matcha", "shinbun",
                "shin'yoh", "supa", "ohsaka", "tohkyoh", "oneesan", "vaiorin", "時々", "isuzu",
                "hawa", "hee", "akana12",
            ],
        ),
        (
            RomanizationName::HepburnTraditional,
            [
                "kya", "sha", "cha", "ja", "ja", "fa", "wo", "gakkō", "matcha", "shimbun",
                "shin-yō", "supa", "ōsaka", "tōkyō", "oneesan", "vaiorin", "時々", "isuzu", "hawa",
                "hee", "akana12",
            ],
        ),
        (
            RomanizationName::HepburnModified,
            [
                "kya", "sha", "cha", "ja", "ja", "fa", "wo", "gakkō", "matcha", "shinbun",
                "shin'yō", "supa", "ōsaka", "tōkyō", "onēsan", "vaiorin", "時々", "isuzu", "hawa",
                "hē", "akana12",
            ],
        ),
        (
            RomanizationName::KunreiSiki,
            [
                "kya", "sya", "tya", "zya", "zya", "ha", "wo", "gakkô", "mattya", "sinbun",
                "sin'yô", "supa", "ôsaka", "tôkyô", "oneesan", "vaiorin", "時々", "isuzu", "hawa",
                "hee", "akana12",
            ],
        ),
    ];
    for (method, expected) in fixtures {
        assert_eq!(romanization::romanization_method(method).name(), method);
        for (word, expected) in words.iter().zip(expected) {
            assert_eq!(
                s(&romanization::romanize_word(&u(word), method, None, true)),
                expected,
                "{} {word}",
                method.as_str()
            );
        }
    }
}

#[test]
fn romanization_normalization_special_and_utf16_fixtures() {
    assert_eq!(
        s(&romanization::romanize_word(
            &u("ﾄｳｷｮｳ"),
            RomanizationName::HepburnTraditional,
            None,
            true
        )),
        "tōkyō"
    );
    assert_eq!(
        s(&romanization::romanize_word(
            &u("ﾄｳｷｮｳ"),
            RomanizationName::HepburnTraditional,
            None,
            false
        )),
        "ﾄｳｷｮｳ"
    );
    assert_eq!(
        s(&romanization::romanize_word(
            &u("かな"),
            RomanizationName::HepburnTraditional,
            Some(&u("っ")),
            true
        )),
        "!"
    );
    assert_eq!(
        s(&romanization::romanize_word(
            &u("かな"),
            RomanizationName::HepburnTraditional,
            Some(&u("ー")),
            true
        )),
        "~"
    );
    for malformed in [vec![0xd83d], vec![0xde00], vec![0xd83d, 0x304b, 0xde00]] {
        let mut expected = malformed.clone();
        if expected.len() == 3 {
            expected[1..2].copy_from_slice(&u("k")[..1]);
            expected.insert(2, b'a' as u16);
        }
        assert_eq!(
            romanization::romanize_word(
                &malformed,
                RomanizationName::HepburnTraditional,
                None,
                true
            ),
            expected
        );
    }
}

#[test]
fn romanization_hints_tree_and_joining_differential_fixtures() {
    assert_eq!(s(&romanization::process_hints(&u("は\u{200c}は"))), "はわ");
    assert_eq!(
        s(&romanization::process_hints(&u("\u{200c}ハ\u{200c}ヘ"))),
        "ワエ"
    );
    assert_eq!(
        s(&romanization::strip_hints(&u("a\u{200b}か\u{200c}な"))),
        "aかな"
    );
    let classes = romanization::get_character_classes(&u("時々"));
    assert_eq!(
        romanization::process_iteration_characters(&classes).len(),
        2
    );
    assert_eq!(
        s(&romanization::join_romanized_parts(&[
            u("Tōkyō"),
            u("to"),
            u(", "),
            u("Ōsaka"),
            vec![],
            u("2026")
        ])),
        "Tōkyō to, Ōsaka 2026",
    );
}
