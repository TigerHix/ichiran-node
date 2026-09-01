//! Portable counter rendering and candidate materialization.
//!
//! This is a direct port of `analyzer-counters.ts` plus the counter candidate
//! boundary in `analyzer.ts`. Text stays in UTF-16 so rendered spans remain
//! identical to JavaScript strings.

use crate::analyzer_lexicon::{
    AnalysisRoot, AnalyzerLexicon, CandidateKind, ConjugationSelection, MaterializedCandidate,
    PhysicalKey, SemanticMember,
};
use crate::analyzer_model::{
    EntryScoreFacts, ScoreCandidate, ScoreWordKind, SequenceFacts, WordScoreFacts,
};
use crate::characters::{CharClass, count_char_class, geminate, get_char_class, rendaku};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;
use crate::numbers::{NumberKana, number_to_kana_with_separator, number_to_kanji, parse_number};
use crate::roots::RootPayload;
use crate::support::{
    SupportCounterClass, SupportCounterVariant, SupportDigit, SupportDigitOption,
};

#[derive(Clone, Debug, PartialEq)]
pub struct CounterValue {
    pub text: Vec<u16>,
    pub reading: Vec<u16>,
    pub number: f64,
    pub route: Route,
    pub value: String,
    pub ordinal: bool,
}

fn utf16(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}

fn append(target: &mut Vec<u16>, value: &str) {
    target.extend(value.encode_utf16());
}

fn digit_of(value: f64) -> i16 {
    let digit = value % 10.0;
    if digit != 0.0 {
        return digit as i16;
    }
    for (current, next) in [
        (10, 100),
        (100, 1_000),
        (1_000, 10_000),
        (10_000, 100_000_000),
    ] {
        if value % f64::from(next) != 0.0 {
            return current;
        }
    }
    digit as i16
}

fn option_for(
    options: &[SupportDigitOption],
    predicate: impl Fn(&SupportDigit) -> bool,
) -> Option<&SupportDigitOption> {
    options.iter().find(|option| predicate(&option.digit))
}

fn class_in(head: &str, classes: &[&str]) -> bool {
    classes.contains(&head)
}

fn replace_number_stem(number: &mut Vec<u16>, digit: i16, replacement: &str) {
    let stem = match digit {
        0 => "れい",
        1 => "いち",
        2 => "に",
        3 => "さん",
        4 => "よん",
        5 => "ご",
        6 => "ろく",
        7 => "なな",
        8 => "はち",
        9 => "きゅう",
        10 => "じゅう",
        100 => "ひゃく",
        1_000 => "せん",
        10_000 => "まん",
        _ => "",
    };
    let keep = number.len().saturating_sub(stem.encode_utf16().count());
    number.truncate(keep);
    append(number, replacement);
}

fn join_counter(
    variant: &SupportCounterVariant,
    value: f64,
    mut number_kana: Vec<u16>,
    mut counter_kana: Vec<u16>,
) -> Vec<u16> {
    const K: &[&str] = &["ka", "ki", "ku", "ke", "ko"];
    const S: &[&str] = &["sa", "shi", "su", "se", "so"];
    const T: &[&str] = &["ta", "chi", "tsu", "te", "to"];
    const H: &[&str] = &["ha", "hi", "fu", "he", "ho"];
    const P: &[&str] = &["pa", "pi", "pu", "pe", "po"];

    let digit = digit_of(value);
    let head = counter_kana
        .first()
        .map(|unit| get_char_class(&[*unit]))
        .unwrap_or_default();
    let head = String::from_utf16_lossy(&head);
    let digit_options = option_for(
        &variant.digit_options,
        |option| matches!(option, SupportDigit::Digit(value) if *value == digit),
    );
    let off = option_for(&variant.digit_options, |option| {
        matches!(option, SupportDigit::Off)
    });

    if off.is_some() || digit_options.is_some() {
        let mut modify_counter = false;
        for option in digit_options
            .map(|option| option.values.as_slice())
            .unwrap_or_default()
        {
            match option.as_str() {
                ":g" => number_kana = geminate(&number_kana),
                ":r" => counter_kana = rendaku(&counter_kana, false),
                ":h" => counter_kana = rendaku(&counter_kana, true),
                ":c" => modify_counter = true,
                value if value.starts_with(':') => {}
                value if modify_counter => counter_kana = utf16(value),
                value => replace_number_stem(&mut number_kana, digit, value),
            }
        }
        number_kana.extend(counter_kana);
        return number_kana;
    }

    let k = class_in(&head, K);
    let s = class_in(&head, S);
    let t = class_in(&head, T);
    let h = class_in(&head, H);
    let p = class_in(&head, P);
    if variant.foreign {
        if (digit == 6 && (k || p))
            || ((digit == 8 || digit == 10) && (k || s || t || p))
            || (digit == 100 && k)
        {
            number_kana = geminate(&number_kana);
        }
        number_kana.extend(counter_kana);
        return number_kana;
    }

    if digit == 1 {
        if k || s || t {
            number_kana = geminate(&number_kana);
        }
        if h {
            number_kana = geminate(&number_kana);
            counter_kana = rendaku(&counter_kana, true);
        }
    } else if digit == 3 {
        if h {
            counter_kana = rendaku(&counter_kana, true);
        }
    } else if matches!(digit, 6 | 8 | 10 | 100) {
        if (digit == 6 && (k || p))
            || ((digit == 8 || digit == 10) && (k || s || t || p))
            || (digit == 100 && k)
        {
            number_kana = geminate(&number_kana);
        }
        if h {
            number_kana = geminate(&number_kana);
            counter_kana = rendaku(&counter_kana, true);
        }
    } else if matches!(digit, 1_000 | 10_000) && h {
        counter_kana = rendaku(&counter_kana, true);
    }
    number_kana.extend(counter_kana);
    number_kana
}

fn hifumi(value: f64) -> &'static str {
    match value as u32 {
        1 => "ひと",
        2 => "ふた",
        3 => "み",
        4 => "よ",
        5 => "いつ",
        6 => "む",
        7 => "なな",
        8 => "や",
        9 => "ここの",
        10 => "と",
        _ => "",
    }
}

fn kun_day(value: f64) -> Option<&'static str> {
    match value as u32 {
        1 => Some("ついたち"),
        2 => Some("ふつか"),
        3 => Some("みっか"),
        4 => Some("よっか"),
        5 => Some("いつか"),
        6 => Some("むいか"),
        7 => Some("なのか"),
        8 => Some("ようか"),
        9 => Some("ここのか"),
        10 => Some("とうか"),
        14 => Some("じゅうよっか"),
        20 => Some("はつか"),
        24 => Some("にじゅうよっか"),
        30 => Some("みそか"),
        _ => None,
    }
}

fn contains_number(values: &[u32], value: f64) -> bool {
    values.iter().any(|item| f64::from(*item) == value)
}

fn valid_counter(variant: &SupportCounterVariant, value: f64, unique: bool) -> bool {
    unique
        && (variant.allowed.is_empty() || contains_number(&variant.allowed, value))
        && match variant.class_name {
            SupportCounterClass::CounterTsu => (1.0..=9.0).contains(&value),
            SupportCounterClass::CounterDaysOn => (value > 10.0 || value == 1.0) && value != 20.0,
            _ => true,
        }
}

fn number_string(value: f64) -> String {
    value.to_string()
}

fn ordinal(value: f64) -> String {
    let suffix = if value % 100.0 > 10.0 && value % 100.0 < 20.0 {
        "th"
    } else if value % 10.0 == 1.0 {
        "st"
    } else if value % 10.0 == 2.0 {
        "nd"
    } else if value % 10.0 == 3.0 {
        "rd"
    } else {
        "th"
    };
    format!("{}{suffix}", number_string(value))
}

fn value_string(variant: &SupportCounterVariant, value: f64) -> String {
    match variant.class_name {
        SupportCounterClass::CounterHalfhour => format!("{}:30", number_string(value)),
        SupportCounterClass::CounterMonths => [
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December",
        ]
        .get((value as usize).wrapping_sub(1))
        .map_or_else(|| number_string(value), |month| (*month).to_owned()),
        SupportCounterClass::CounterWari => format!("{}%", number_string(value * 10.0)),
        _ => {
            let value = if variant.ordinal {
                ordinal(value)
            } else {
                number_string(value)
            };
            let descriptions = variant.suffix_descriptions.iter().rev().fold(
                String::new(),
                |mut output, description| {
                    output.push(' ');
                    output.push_str(description);
                    output
                },
            );
            format!("Value: {value}{descriptions}")
        }
    }
}

fn number_kana(value: f64) -> Result<Vec<u16>> {
    match number_to_kana_with_separator(value, Some(&[b' ' as u16]))
        .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))?
    {
        NumberKana::Joined(value) => Ok(value),
        NumberKana::Groups(_) => Err(KernelError::new(
            ErrorCode::Internal,
            "number reading unexpectedly retained separate groups",
        )),
    }
}

pub fn materialize_counter(
    number_text: &[u16],
    variant: &SupportCounterVariant,
    unique: bool,
) -> Result<Option<CounterValue>> {
    let Ok(value) = parse_number(number_text) else {
        return Ok(None);
    };
    if !valid_counter(variant, value, unique) {
        return Ok(None);
    }

    let mut reading = match variant.class_name {
        SupportCounterClass::NumberText => number_kana(value)?,
        SupportCounterClass::CounterTsu => [
            "",
            "ひとつ",
            "ふたつ",
            "みっつ",
            "よっつ",
            "いつつ",
            "むっつ",
            "ななつ",
            "やっつ",
            "ここのつ",
        ]
        .get(value as usize)
        .map_or_else(Vec::new, |value| utf16(value)),
        SupportCounterClass::CounterHifumi if contains_number(&variant.digit_set, value) => {
            let mut value = utf16(hifumi(value));
            append(&mut value, &variant.kana);
            value
        }
        SupportCounterClass::CounterDaysKun => kun_day(value).map_or_else(
            || {
                Ok(join_counter(
                    variant,
                    value,
                    number_kana(value)?,
                    utf16(&variant.kana),
                ))
            },
            |value| Ok(utf16(value)),
        )?,
        SupportCounterClass::CounterPeople if value == 1.0 || value == 2.0 => {
            utf16(if value == 1.0 {
                "ひとり"
            } else {
                "ふたり"
            })
        }
        SupportCounterClass::CounterAge if value == 20.0 => utf16("はたち"),
        _ => join_counter(variant, value, number_kana(value)?, utf16(&variant.kana)),
    };
    if let Some(suffix) = &variant.suffix {
        append(&mut reading, suffix);
    }
    let mut text = number_text.to_vec();
    append(&mut text, &variant.text);
    Ok(Some(CounterValue {
        route: if count_char_class(&text, CharClass::KanjiChar) > 0 {
            Route::Kanji
        } else {
            Route::Kana
        },
        text,
        reading,
        number: value,
        value: value_string(variant, value),
        ordinal: variant.ordinal,
    }))
}

pub fn counter_kanji(value: f64, counter_text: &[u16]) -> Result<Vec<u16>> {
    let mut result = number_to_kanji(value)
        .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))?;
    result.extend_from_slice(counter_text);
    Ok(result)
}

fn sequence_facts(roots: &RootPayload, entry: Option<usize>) -> Result<SequenceFacts> {
    let Some(entry) = entry else {
        return Ok(SequenceFacts::default());
    };
    Ok(SequenceFacts {
        all_archived: roots.entry_archived(entry)?,
        prefer_kana: roots.entry_prefer_kana(entry)?,
        prefer_kana_on_ordinal_zero: roots.entry_prefer_kana_zero(entry)?,
    })
}

pub fn materialize_counter_candidate(
    roots: &RootPayload,
    lexicon: &mut AnalyzerLexicon<'_>,
    rendered: CounterValue,
    variant: &SupportCounterVariant,
) -> Result<MaterializedCandidate> {
    let source_seq = variant.source.as_ref().map(|source| source.seq);
    let entry_index = source_seq
        .map(|seq| roots.find_entry_index(seq))
        .transpose()?
        .flatten();
    let facts = sequence_facts(roots, entry_index)?;
    let mut common = variant.common.map(i32::from);
    let mut ord = variant
        .source
        .as_ref()
        .map_or(0, |source| i32::from(source.ord));
    let mut nokanji = false;
    let mut root = None;
    if let (Some(source), Some(_)) = (&variant.source, entry_index) {
        let direct = lexicon
            .lexical(&utf16(&source.text))?
            .into_iter()
            .find(|value| value.public_seq == source_seq && value.inflection.is_empty());
        if let Some(ScoreCandidate::Word(word)) = direct.as_ref().map(|value| &value.score_facts) {
            if common.is_none() {
                common = word.common;
            }
            nokanji = word.nokanji;
        }
        root = direct.and_then(|value| value.root).or_else(|| {
            Some(AnalysisRoot {
                seq: source.seq,
                form: utf16(&source.text),
                reading: utf16(&source.text),
            })
        });
        ord = i32::from(source.ord);
    }

    let entry = entry_index
        .map(|entry| {
            Ok(EntryScoreFacts {
                root: true,
                n_kanji: u32::from(roots.entry_n_kanji(entry)?),
                primary_nokanji: roots.entry_primary_nokanji(entry)?,
            })
        })
        .transpose()?;
    let target_n_kanji = entry_index
        .map(|entry| roots.entry_n_kanji(entry).map(u16::from))
        .transpose()?;
    let target_n_kana = entry_index
        .map(|entry| roots.entry_n_kana(entry).map(u16::from))
        .transpose()?;
    let score_facts = ScoreCandidate::Word(WordScoreFacts {
        kind: ScoreWordKind::Counter,
        text: rendered.text.clone(),
        true_text: rendered.text.clone(),
        true_text_follows_text: true,
        route: rendered.route,
        seq: source_seq.map(i64::from),
        ord,
        common,
        nokanji,
        entry,
        conjugation_only: false,
        conjugations: Vec::new(),
        positions: vec!["ctr".to_owned()],
        self_facts: facts,
        lineage: facts,
        inherited_common: None,
        inherited_ord: None,
        split: None,
        suru_break: None,
    });
    let physical_key = source_seq.map_or_else(
        || PhysicalKey::Counter(rendered.text.clone()),
        PhysicalKey::Sequence,
    );
    Ok(MaterializedCandidate {
        kind: CandidateKind::Counter,
        text: rendered.text.clone(),
        true_text: rendered.text,
        route: rendered.route,
        reading: rendered.reading,
        public_seq: source_seq,
        physical_seq: source_seq.map(i64::from),
        physical_key,
        physical_group: None,
        lookup_locators: Vec::new(),
        member_ord: None,
        entry_index,
        root: root.clone(),
        inflection: Vec::new(),
        score_facts,
        components: Vec::new(),
        counter: Some((rendered.value, rendered.ordinal)),
        suffix_class: None,
        definition_seq: source_seq,
        semantic_members: vec![SemanticMember {
            entry_index,
            root,
            inflection: Vec::new(),
            public_seq: source_seq,
            physical_group: None,
            member_ord: None,
            target_n_kanji,
            target_n_kana,
            via_seq: None,
            stage_groups: Vec::new(),
            stage_keys: Vec::new(),
            stage_member_ords: Vec::new(),
            stage_prop_ords: Vec::new(),
        }],
        identity_roots: Vec::new(),
        conjugation_selection: ConjugationSelection::Default,
    })
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use super::*;
    use crate::pack::Pack;
    use crate::support::AnalyzerSupport;

    fn variant() -> SupportCounterVariant {
        SupportCounterVariant {
            class_name: SupportCounterClass::CounterText,
            text: "本".to_owned(),
            kana: "ほん".to_owned(),
            suffix: None,
            source: None,
            ordinal: false,
            foreign: false,
            common: None,
            suffix_descriptions: Vec::new(),
            digit_options: Vec::new(),
            digit_set: Vec::new(),
            allowed: Vec::new(),
        }
    }

    fn reading(number: &str, variant: &SupportCounterVariant) -> Option<String> {
        materialize_counter(&utf16(number), variant, true)
            .unwrap()
            .map(|value| String::from_utf16_lossy(&value.reading))
    }

    #[test]
    fn standard_and_special_readings_match_typescript() {
        assert_eq!(reading("1", &variant()).as_deref(), Some("いっぽん"));
        assert_eq!(reading("3", &variant()).as_deref(), Some("さんぽん"));
        assert_eq!(reading("6", &variant()).as_deref(), Some("ろっぽん"));

        let mut people = variant();
        people.class_name = SupportCounterClass::CounterPeople;
        people.text = "人".to_owned();
        people.kana = "にん".to_owned();
        assert_eq!(reading("2", &people).as_deref(), Some("ふたり"));
    }

    #[test]
    fn digit_options_and_validity_match_typescript() {
        let mut hour = variant();
        hour.text = "時".to_owned();
        hour.kana = "じ".to_owned();
        hour.digit_options = vec![SupportDigitOption {
            digit: SupportDigit::Digit(4),
            values: vec!["よ".to_owned()],
        }];
        assert_eq!(reading("4", &hour).as_deref(), Some("よじ"));

        let mut tsu = variant();
        tsu.class_name = SupportCounterClass::CounterTsu;
        tsu.text = "つ".to_owned();
        tsu.kana = "つ".to_owned();
        assert_eq!(reading("10", &tsu), None);

        let mut restricted = variant();
        restricted.allowed = vec![1, 2];
        assert_eq!(reading("3", &restricted), None);
    }

    #[test]
    fn preserves_ordered_digit_mutations_routes_values_and_utf16_lengths() {
        let mut custom = variant();
        custom.text = "つ😀".to_owned();
        custom.kana = "ほん".to_owned();
        custom.ordinal = true;
        custom.suffix_descriptions = vec!["A".to_owned(), "B".to_owned()];
        custom.digit_options = vec![SupportDigitOption {
            digit: SupportDigit::Digit(1),
            values: vec![":g".to_owned(), ":c".to_owned(), "ぽん".to_owned()],
        }];
        let value = materialize_counter(&utf16("1"), &custom, true)
            .unwrap()
            .unwrap();
        assert_eq!(String::from_utf16_lossy(&value.reading), "いっぽん");
        assert_eq!(value.text.len(), 4);
        assert_eq!(value.route, Route::Kana);
        assert_eq!(value.value, "Value: 1st B A");

        let mut kanji = variant();
        kanji.class_name = SupportCounterClass::CounterMonths;
        let value = materialize_counter(&utf16("12"), &kanji, true)
            .unwrap()
            .unwrap();
        assert_eq!(value.value, "December");
        assert_eq!(
            String::from_utf16_lossy(&counter_kanji(12.0, &utf16("本")).unwrap()),
            "十二本"
        );
    }

    #[test]
    fn all_special_classes_match_authoritative_typescript_witnesses() {
        let cases = [
            (
                "123",
                SupportCounterClass::NumberText,
                "",
                "",
                "ひゃく にじゅう さん",
                "Value: 123",
            ),
            (
                "10",
                SupportCounterClass::CounterHifumi,
                "日",
                "か",
                "とか",
                "Value: 10",
            ),
            (
                "14",
                SupportCounterClass::CounterDaysKun,
                "日",
                "にち",
                "じゅうよっか",
                "Value: 14",
            ),
            (
                "11",
                SupportCounterClass::CounterDaysOn,
                "日",
                "にち",
                "じゅう いちにち",
                "Value: 11",
            ),
            (
                "2",
                SupportCounterClass::CounterMonths,
                "月",
                "がつ",
                "にがつ",
                "February",
            ),
            (
                "1",
                SupportCounterClass::CounterPeople,
                "人",
                "にん",
                "ひとり",
                "Value: 1",
            ),
            (
                "2",
                SupportCounterClass::CounterWari,
                "割",
                "わり",
                "にわり",
                "20%",
            ),
            (
                "20",
                SupportCounterClass::CounterAge,
                "歳",
                "さい",
                "はたち",
                "Value: 20",
            ),
            (
                "2",
                SupportCounterClass::CounterHalfhour,
                "時半",
                "じはん",
                "にじはん",
                "2:30",
            ),
        ];
        for (number, class_name, text, kana, expected_reading, expected_value) in cases {
            let mut value = variant();
            value.class_name = class_name;
            value.text = text.to_owned();
            value.kana = kana.to_owned();
            if class_name == SupportCounterClass::CounterHifumi {
                value.digit_set = vec![10];
            }
            let rendered = materialize_counter(&utf16(number), &value, true)
                .unwrap()
                .unwrap();
            assert_eq!(
                String::from_utf16_lossy(&rendered.reading),
                expected_reading
            );
            assert_eq!(rendered.value, expected_value);
        }

        let mut tsu = variant();
        tsu.class_name = SupportCounterClass::CounterTsu;
        assert!(
            materialize_counter(&utf16("10"), &tsu, true)
                .unwrap()
                .is_none()
        );

        let mut days = variant();
        days.class_name = SupportCounterClass::CounterDaysOn;
        assert!(
            materialize_counter(&utf16("10"), &days, true)
                .unwrap()
                .is_none()
        );
        assert!(
            materialize_counter(&utf16("20"), &days, true)
                .unwrap()
                .is_none()
        );
    }

    #[test]
    fn off_foreign_and_ordered_counter_directives_match_typescript() {
        let mut off = variant();
        off.digit_options = vec![SupportDigitOption {
            digit: SupportDigit::Off,
            values: Vec::new(),
        }];
        assert_eq!(reading("1", &off).as_deref(), Some("いちほん"));

        let mut foreign = variant();
        foreign.text = "ケース".to_owned();
        foreign.kana = "けーす".to_owned();
        foreign.foreign = true;
        assert_eq!(reading("8", &foreign).as_deref(), Some("はっけーす"));

        let mut ordered = variant();
        ordered.text = "個".to_owned();
        ordered.kana = "こ".to_owned();
        ordered.digit_options = vec![SupportDigitOption {
            digit: SupportDigit::Digit(3),
            values: vec![
                "さ".to_owned(),
                ":c".to_owned(),
                "ほん".to_owned(),
                ":r".to_owned(),
            ],
        }];
        assert_eq!(reading("3", &ordered).as_deref(), Some("さぼん"));
        assert!(
            materialize_counter(&utf16("3"), &ordered, false)
                .unwrap()
                .is_none()
        );
        assert!(
            materialize_counter(&utf16("x"), &ordered, true)
                .unwrap()
                .is_none()
        );
    }

    #[test]
    #[ignore = "requires the digest-locked portable-core-260118-baseline release"]
    fn qualified_pack_counter_order_and_rendering_match_typescript() {
        let directory = std::env::var_os("ICHIRAN_M1_PACK_DIR")
            .map(PathBuf::from)
            .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory");
        let pack = Pack::open(fs::read(directory.join("hot.bin")).unwrap()).unwrap();
        let support = AnalyzerSupport::open(pack.section_data(4).unwrap()).unwrap();
        let cases = [
            (
                "本目",
                vec![
                    (SupportCounterClass::CounterHifumi, Some(1_260_670)),
                    (SupportCounterClass::CounterText, Some(1_522_150)),
                ],
                vec![
                    Some(("みもとめ", "Value: 3rd")),
                    Some(("さんぼんめ", "Value: 3rd")),
                ],
            ),
            (
                "日",
                vec![
                    (SupportCounterClass::CounterDaysOn, Some(2_083_100)),
                    (SupportCounterClass::CounterDaysKun, Some(2_083_110)),
                    (SupportCounterClass::CounterText, Some(2_856_786)),
                ],
                vec![
                    None,
                    Some(("みっか", "Value: 3")),
                    Some(("さんんち", "Value: 3")),
                ],
            ),
            (
                "",
                vec![(SupportCounterClass::NumberText, None)],
                vec![Some(("さん", "Value: 3"))],
            ),
        ];
        for (key, expected_variants, expected_values) in cases {
            let variants = support.counters(&utf16(key)).unwrap();
            assert_eq!(
                variants
                    .iter()
                    .map(|value| (
                        value.class_name,
                        value.source.as_ref().map(|source| source.seq)
                    ))
                    .collect::<Vec<_>>(),
                expected_variants
            );
            let values = variants
                .iter()
                .map(|variant| {
                    materialize_counter(&utf16("3"), variant, true)
                        .unwrap()
                        .map(|value| (String::from_utf16_lossy(&value.reading), value.value))
                })
                .collect::<Vec<_>>();
            assert_eq!(
                values,
                expected_values
                    .into_iter()
                    .map(|value| value.map(|(reading, value)| (reading.into(), value.into())))
                    .collect::<Vec<_>>()
            );
        }
    }
}
