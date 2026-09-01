//! Host-neutral Japanese character utilities.
//!
//! Inputs and outputs use UTF-16 code units so offsets, slicing, astral text,
//! and unpaired surrogates have the same semantics as JavaScript strings.

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CharClass {
    Katakana,
    KatakanaUniq,
    Hiragana,
    Kanji,
    KanjiChar,
    Kana,
    Traditional,
    Nonword,
    Number,
}

impl CharClass {
    pub const ALL: [Self; 9] = [
        Self::Katakana,
        Self::KatakanaUniq,
        Self::Hiragana,
        Self::Kanji,
        Self::KanjiChar,
        Self::Kana,
        Self::Traditional,
        Self::Nonword,
        Self::Number,
    ];
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BasicSplitType {
    Word,
    Misc,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicSplitSegment {
    pub kind: BasicSplitType,
    pub text: Vec<u16>,
}

pub const SOKUON_CHARACTERS: &[(&str, &str)] = &[("sokuon", "っッ")];
pub const ITERATION_CHARACTERS: &[(&str, &str)] = &[("iter", "ゝヽ"), ("iterV", "ゞヾ")];
pub const MODIFIER_CHARACTERS: &[(&str, &str)] = &[
    ("+a", "ぁァ"),
    ("+i", "ぃィ"),
    ("+u", "ぅゥ"),
    ("+e", "ぇェ"),
    ("+o", "ぉォ"),
    ("+ya", "ゃャ"),
    ("+yu", "ゅュ"),
    ("+yo", "ょョ"),
    ("+wa", "ゎヮ"),
    ("longVowel", "ー"),
];
pub const KANA_CHARACTERS: &[(&str, &str)] = &[
    ("a", "あア"),
    ("i", "いイ"),
    ("u", "うウ"),
    ("e", "えエ"),
    ("o", "おオ"),
    ("ka", "かカ"),
    ("ki", "きキ"),
    ("ku", "くク"),
    ("ke", "けケ"),
    ("ko", "こコ"),
    ("sa", "さサ"),
    ("shi", "しシ"),
    ("su", "すス"),
    ("se", "せセ"),
    ("so", "そソ"),
    ("ta", "たタ"),
    ("chi", "ちチ"),
    ("tsu", "つツ"),
    ("te", "てテ"),
    ("to", "とト"),
    ("na", "なナ"),
    ("ni", "にニ"),
    ("nu", "ぬヌ"),
    ("ne", "ねネ"),
    ("no", "のノ"),
    ("ha", "はハ"),
    ("hi", "ひヒ"),
    ("fu", "ふフ"),
    ("he", "へヘ"),
    ("ho", "ほホ"),
    ("ma", "まマ"),
    ("mi", "みミ"),
    ("mu", "むム"),
    ("me", "めメ"),
    ("mo", "もモ"),
    ("ya", "やヤ"),
    ("yu", "ゆユ"),
    ("yo", "よヨ"),
    ("ra", "らラ"),
    ("ri", "りリ"),
    ("ru", "るル"),
    ("re", "れレ"),
    ("ro", "ろロ"),
    ("wa", "わワ"),
    ("wi", "ゐヰ"),
    ("we", "ゑヱ"),
    ("wo", "をヲ"),
    ("n", "んン"),
    ("ga", "がガ"),
    ("gi", "ぎギ"),
    ("gu", "ぐグ"),
    ("ge", "げゲ"),
    ("go", "ごゴ"),
    ("za", "ざザ"),
    ("ji", "じジ"),
    ("zu", "ずズ"),
    ("ze", "ぜゼ"),
    ("zo", "ぞゾ"),
    ("da", "だダ"),
    ("dji", "ぢヂ"),
    ("dzu", "づヅ"),
    ("de", "でデ"),
    ("do", "どド"),
    ("ba", "ばバ"),
    ("bi", "びビ"),
    ("bu", "ぶブ"),
    ("be", "べベ"),
    ("bo", "ぼボ"),
    ("pa", "ぱパ"),
    ("pi", "ぴピ"),
    ("pu", "ぷプ"),
    ("pe", "ぺペ"),
    ("po", "ぽポ"),
    ("vu", "ゔヴ"),
];

pub const DAKUTEN_HASH: &[(&str, &str)] = &[
    ("ka", "ga"),
    ("ki", "gi"),
    ("ku", "gu"),
    ("ke", "ge"),
    ("ko", "go"),
    ("sa", "za"),
    ("shi", "ji"),
    ("su", "zu"),
    ("se", "ze"),
    ("so", "zo"),
    ("ta", "da"),
    ("chi", "dji"),
    ("tsu", "dzu"),
    ("te", "de"),
    ("to", "do"),
    ("ha", "ba"),
    ("hi", "bi"),
    ("fu", "bu"),
    ("he", "be"),
    ("ho", "bo"),
    ("u", "vu"),
];
pub const HANDAKUTEN_HASH: &[(&str, &str)] = &[
    ("ha", "pa"),
    ("hi", "pi"),
    ("fu", "pu"),
    ("he", "pe"),
    ("ho", "po"),
];
pub const UNDAKUTEN_HASH: &[(&str, &str)] = &[
    ("ga", "ka"),
    ("gi", "ki"),
    ("gu", "ku"),
    ("ge", "ke"),
    ("go", "ko"),
    ("za", "sa"),
    ("ji", "shi"),
    ("zu", "su"),
    ("ze", "se"),
    ("zo", "so"),
    ("da", "ta"),
    ("dji", "chi"),
    ("dzu", "tsu"),
    ("de", "te"),
    ("do", "to"),
    ("ba", "ha"),
    ("bi", "hi"),
    ("bu", "fu"),
    ("be", "he"),
    ("bo", "ho"),
    ("pa", "ha"),
    ("pi", "hi"),
    ("pu", "fu"),
    ("pe", "he"),
    ("po", "ho"),
    ("vu", "u"),
];

pub const PUNCTUATION_MARKS: &[(&str, &str)] = &[
    ("【", " ["),
    ("】", "] "),
    ("、", ", "),
    ("，", ", "),
    ("。", ". "),
    ("・・・", "... "),
    ("・", " "),
    ("　", " "),
    ("「", " \""),
    ("」", "\" "),
    ("゛", "\""),
    ("『", " «"),
    ("』", "» "),
    ("〜", " - "),
    ("：", ": "),
    ("！", "! "),
    ("？", "? "),
    ("；", "; "),
];

pub const HALF_WIDTH_KANA: &str = "･ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ";
pub const FULL_WIDTH_KANA: &str = "・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜";
pub const ABNORMAL_CHARS: &str = "０１２３４５６７８９ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ＃＄％＆（）＊＋／〈＝〉？＠［］＾＿'｛｜｝～･ｦｧｨｩｪｫｬｭｮｯｰｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜﾝﾞﾟ";
pub const NORMAL_CHARS: &str = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ#$%&()*+/<=>?@[]^_`{|}~・ヲァィゥェォャュョッーアイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワン゛゜";

pub fn all_characters() -> impl Iterator<Item = (&'static str, &'static str)> {
    SOKUON_CHARACTERS
        .iter()
        .chain(ITERATION_CHARACTERS)
        .chain(MODIFIER_CHARACTERS)
        .chain(KANA_CHARACTERS)
        .copied()
}

pub(crate) fn class_for_unit(unit: u16) -> Option<&'static str> {
    all_characters().find_map(|(class, characters)| {
        characters
            .encode_utf16()
            .any(|item| item == unit)
            .then_some(class)
    })
}

pub(crate) fn characters_for_class(class: &str) -> Option<&'static str> {
    all_characters().find_map(|(name, characters)| (name == class).then_some(characters))
}

pub(crate) fn is_kana_class(class: &str) -> bool {
    all_characters().any(|(name, _)| name == class)
}

pub fn character_class(unit: u16) -> Option<&'static str> {
    class_for_unit(unit)
}

fn mapped_class<'a>(class: &str, table: &'a [(&'a str, &'a str)]) -> Option<&'a str> {
    table
        .iter()
        .find_map(|(from, to)| (*from == class).then_some(*to))
}

pub fn get_char_class(character: &[u16]) -> Vec<u16> {
    if let [unit] = character
        && let Some(class) = class_for_unit(*unit)
    {
        return class.encode_utf16().collect();
    }
    character.to_vec()
}

pub fn voice_char(char_class: &str) -> String {
    mapped_class(char_class, DAKUTEN_HASH)
        .unwrap_or(char_class)
        .to_owned()
}

pub fn long_vowel_modifier_p(modifier: &str, previous_character: &[u16]) -> bool {
    let vowel = match modifier {
        "+a" => 'A',
        "+i" => 'I',
        "+u" => 'U',
        "+e" => 'E',
        "+o" => 'O',
        _ => return false,
    };
    let [unit] = previous_character else {
        return false;
    };
    let Some(class) = class_for_unit(*unit) else {
        return false;
    };
    class
        .chars()
        .last()
        .is_some_and(|item| item.to_ascii_uppercase() == vowel)
}

fn matches_class(unit: u16, char_class: CharClass) -> bool {
    let katakana_uniq = (0x30a1..=0x30fa).contains(&unit) || (0x30fd..=0x30fe).contains(&unit);
    let hiragana_uniq = (0x3041..=0x3094).contains(&unit) || (0x309d..=0x309e).contains(&unit);
    let kanji_char = (0x4e00..=0x9faf).contains(&unit);
    let kanji = kanji_char || matches!(unit, 0x3005 | 0x30f6 | 0x3006);
    let kana = katakana_uniq || hiragana_uniq || unit == 0x30fc;
    match char_class {
        CharClass::Katakana => katakana_uniq || unit == 0x30fc,
        CharClass::KatakanaUniq => katakana_uniq,
        CharClass::Hiragana => hiragana_uniq || unit == 0x30fc,
        CharClass::Kanji => kanji,
        CharClass::KanjiChar => kanji_char,
        CharClass::Kana => kana,
        CharClass::Traditional => hiragana_uniq || unit == 0x30fc || kanji,
        CharClass::Nonword => !(kanji || kana || unit == 0x3007),
        CharClass::Number => {
            (u16::from(b'0')..=u16::from(b'9')).contains(&unit)
                || (0xff10..=0xff19).contains(&unit)
                || "〇一二三四五六七八九零壱弐参拾十百千万億兆京"
                    .encode_utf16()
                    .any(|item| item == unit)
        }
    }
}

pub fn test_word(word: &[u16], char_class: CharClass) -> bool {
    !word.is_empty() && word.iter().all(|unit| matches_class(*unit, char_class))
}

pub fn count_char_class(word: &[u16], char_class: CharClass) -> usize {
    word.iter()
        .filter(|unit| matches_class(**unit, char_class))
        .count()
}

pub fn collect_char_class(word: &[u16], char_class: CharClass) -> Vec<Vec<u16>> {
    word.iter()
        .filter(|unit| matches_class(**unit, char_class))
        .map(|unit| vec![*unit])
        .collect()
}

pub fn sequential_kanji_positions(word: &[u16], offset: usize) -> Vec<usize> {
    word.windows(2)
        .enumerate()
        .filter(|(_, pair)| {
            pair.iter()
                .all(|unit| *unit == 0x3005 || (0x4e00..=0x9faf).contains(unit))
        })
        .map(|(index, _)| index + 1 + offset)
        .collect()
}

fn append_replacement(
    output: &mut Vec<u16>,
    replacement: &[u16],
    matched: &[u16],
    prefix: &[u16],
    suffix: &[u16],
) {
    let mut index = 0;
    while index < replacement.len() {
        if replacement[index] == b'$' as u16 && index + 1 < replacement.len() {
            match replacement[index + 1] {
                value if value == b'$' as u16 => output.push(b'$' as u16),
                value if value == b'&' as u16 => output.extend_from_slice(matched),
                value if value == b'`' as u16 => output.extend_from_slice(prefix),
                value if value == b'\'' as u16 => output.extend_from_slice(suffix),
                _ => {
                    output.push(replacement[index]);
                    index += 1;
                    output.push(replacement[index]);
                }
            }
            index += 2;
        } else {
            output.push(replacement[index]);
            index += 1;
        }
    }
}

fn replace_all(input: &[u16], from: &[u16], to: &[u16]) -> Vec<u16> {
    let mut output = Vec::new();
    if from.is_empty() {
        let mut index = 0;
        loop {
            append_replacement(&mut output, to, &[], &input[..index], &input[index..]);
            if index == input.len() {
                break;
            }
            let width = if (0xd800..=0xdbff).contains(&input[index])
                && input
                    .get(index + 1)
                    .is_some_and(|unit| (0xdc00..=0xdfff).contains(unit))
            {
                2
            } else {
                1
            };
            output.extend_from_slice(&input[index..index + width]);
            index += width;
        }
        return output;
    }
    let mut index = 0;
    while index < input.len() {
        if input[index..].starts_with(from) {
            let end = index + from.len();
            append_replacement(&mut output, to, from, &input[..index], &input[end..]);
            index = end;
        } else {
            output.push(input[index]);
            index += 1;
        }
    }
    output
}

pub fn simplify_ngrams(input: &[u16], replacements: &[(Vec<u16>, Vec<u16>)]) -> Vec<u16> {
    replacements
        .iter()
        .fold(input.to_vec(), |result, (from, to)| {
            replace_all(&result, from, to)
        })
}

fn normal_unit(unit: u16, kana_only: bool) -> Option<u16> {
    let source = if kana_only {
        HALF_WIDTH_KANA
    } else {
        ABNORMAL_CHARS
    };
    let target = if kana_only {
        FULL_WIDTH_KANA
    } else {
        NORMAL_CHARS
    };
    source
        .encode_utf16()
        .position(|item| item == unit)
        .and_then(|index| target.encode_utf16().nth(index))
}

pub fn dakuten_join() -> Vec<(Vec<u16>, Vec<u16>)> {
    let mut result = Vec::new();
    for (table, mark) in [(DAKUTEN_HASH, 0x309b), (HANDAKUTEN_HASH, 0x309c)] {
        for (plain_class, voiced_class) in table {
            let Some(plain) = characters_for_class(plain_class) else {
                continue;
            };
            let Some(voiced) = characters_for_class(voiced_class) else {
                continue;
            };
            let plain_units: Vec<u16> = plain.encode_utf16().collect();
            let voiced_units: Vec<u16> = voiced.encode_utf16().collect();
            let start = plain_units.len().saturating_sub(voiced_units.len());
            for (plain_unit, voiced_unit) in plain_units[start..].iter().zip(voiced_units) {
                result.push((vec![*plain_unit, mark], vec![voiced_unit]));
            }
        }
    }
    result
}

pub fn normalize(input: &[u16], kana_only: bool, skip_punctuation: bool) -> Vec<u16> {
    let normalized: Vec<u16> = input
        .iter()
        .map(|unit| normal_unit(*unit, kana_only).unwrap_or(*unit))
        .collect();
    let mut replacements = Vec::new();
    if !kana_only && !skip_punctuation {
        replacements.extend(
            PUNCTUATION_MARKS
                .iter()
                .map(|(from, to)| (from.encode_utf16().collect(), to.encode_utf16().collect())),
        );
    }
    replacements.extend(dakuten_join());
    simplify_ngrams(&normalized, &replacements)
}

fn is_decimal(unit: u16) -> bool {
    (u16::from(b'0')..=u16::from(b'9')).contains(&unit)
        || (0xff10..=0xff19).contains(&unit)
        || unit == 0x3007
}

fn is_traditional(unit: u16) -> bool {
    matches_class(unit, CharClass::Kana) || matches_class(unit, CharClass::Kanji) || unit == 0x3007
}

fn basic_match(input: &[u16], start: usize) -> Option<usize> {
    let first = *input.get(start)?;
    if !is_traditional(first)
        && (!is_decimal(first)
            || start.checked_sub(1).is_some_and(|index| {
                matches!(input[index], 0x2e | 0x2c) || is_decimal(input[index])
            }))
    {
        return None;
    }
    let mut end = start;
    let mut last_traditional = None;
    while end < input.len() && (is_decimal(input[end]) || is_traditional(input[end])) {
        if is_traditional(input[end]) {
            last_traditional = Some(end + 1);
        }
        end += 1;
    }
    last_traditional
}

pub fn basic_split(input: &[u16]) -> Vec<BasicSplitSegment> {
    let mut pieces = Vec::new();
    let mut last = 0;
    let mut scan = 0;
    while scan < input.len() {
        if let Some(end) = basic_match(input, scan) {
            if scan > last {
                pieces.push(input[last..scan].to_vec());
            }
            pieces.push(input[scan..end].to_vec());
            last = end;
            scan = end;
        } else {
            scan += 1;
        }
    }
    if last < input.len() {
        pieces.push(input[last..].to_vec());
    }
    let mut misc = false;
    pieces
        .into_iter()
        .enumerate()
        .filter(|(_, text)| !text.is_empty())
        .map(|(index, text)| {
            misc = if index == 0 {
                test_word(&text, CharClass::Nonword)
            } else {
                !misc
            };
            BasicSplitSegment {
                kind: if misc {
                    BasicSplitType::Misc
                } else {
                    BasicSplitType::Word
                },
                text,
            }
        })
        .collect()
}

pub fn mora_length(input: &[u16]) -> usize {
    const IGNORED: &str = "っッぁァぃィぅゥぇェぉォゃャゅュょョー";
    let mut length = 0;
    let mut offset = 0;
    while offset < input.len() {
        let paired = (0xd800..=0xdbff).contains(&input[offset])
            && input
                .get(offset + 1)
                .is_some_and(|unit| (0xdc00..=0xdfff).contains(unit));
        if paired || !IGNORED.encode_utf16().any(|item| item == input[offset]) {
            length += 1;
        }
        offset += if paired { 2 } else { 1 };
    }
    length
}

fn convert_kana(input: &[u16], katakana: bool) -> Vec<u16> {
    let mut output = Vec::with_capacity(input.len());
    let mut offset = 0;
    while offset < input.len() {
        let width = if (0xd800..=0xdbff).contains(&input[offset])
            && input
                .get(offset + 1)
                .is_some_and(|unit| (0xdc00..=0xdfff).contains(unit))
        {
            2
        } else {
            1
        };
        let original = &input[offset..offset + width];
        let normalized = (width == 1)
            .then(|| normal_unit(input[offset], false))
            .flatten();
        let unit = normalized.unwrap_or(input[offset]);
        let converted = class_for_unit(unit)
            .and_then(characters_for_class)
            .and_then(|characters| {
                if katakana {
                    characters.encode_utf16().last()
                } else {
                    characters.encode_utf16().next()
                }
            });
        if let Some(unit) = converted {
            output.push(unit);
        } else {
            output.extend_from_slice(original);
        }
        offset += width;
    }
    output
}

pub fn as_hiragana(input: &[u16]) -> Vec<u16> {
    convert_kana(input, false)
}
pub fn as_katakana(input: &[u16]) -> Vec<u16> {
    convert_kana(input, true)
}

pub fn consecutive_char_groups(
    char_class: CharClass,
    input: &[u16],
    start: usize,
    end: usize,
) -> Vec<(usize, usize)> {
    let end = end.min(input.len());
    let mut groups = Vec::new();
    let mut index = start.min(end);
    while index < end {
        if !matches_class(input[index], char_class) {
            index += 1;
            continue;
        }
        let group_start = index;
        while index < end && matches_class(input[index], char_class) {
            index += 1;
        }
        groups.push((group_start, index));
    }
    groups
}

fn replace_initial_kana(input: &[u16], table: &[(&str, &str)]) -> Vec<u16> {
    let Some(&first) = input.first() else {
        return Vec::new();
    };
    let Some(source_class) = class_for_unit(first) else {
        return input.to_vec();
    };
    let Some(target_class) = mapped_class(source_class, table) else {
        return input.to_vec();
    };
    let Some(source) = characters_for_class(source_class) else {
        return input.to_vec();
    };
    let Some(target) = characters_for_class(target_class) else {
        return input.to_vec();
    };
    let Some(index) = source.encode_utf16().position(|unit| unit == first) else {
        return input.to_vec();
    };
    let Some(replacement) = target.encode_utf16().nth(index) else {
        return input.to_vec();
    };
    let mut output = input.to_vec();
    output[0] = replacement;
    output
}

pub fn unrendaku(input: &[u16]) -> Vec<u16> {
    replace_initial_kana(input, UNDAKUTEN_HASH)
}
pub fn rendaku(input: &[u16], handakuten: bool) -> Vec<u16> {
    replace_initial_kana(
        input,
        if handakuten {
            HANDAKUTEN_HASH
        } else {
            DAKUTEN_HASH
        },
    )
}

pub fn geminate(input: &[u16]) -> Vec<u16> {
    if input.is_empty() {
        return Vec::new();
    }
    let mut output = input[..input.len() - 1].to_vec();
    output.push(0x3063);
    output
}

pub fn destem(word: &[u16], stem: usize, char_class: CharClass) -> Vec<u16> {
    if stem == 0 {
        return word.to_vec();
    }
    let positions: Vec<usize> = word
        .iter()
        .enumerate()
        .filter_map(|(index, unit)| matches_class(*unit, char_class).then_some(index))
        .collect();
    positions
        .get(positions.len().wrapping_sub(stem))
        .map_or_else(Vec::new, |position| word[..*position].to_vec())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn u(value: &str) -> Vec<u16> {
        value.encode_utf16().collect()
    }

    #[test]
    fn preserves_javascript_replacement_and_utf16_edges() {
        assert_eq!(
            simplify_ngrams(&u("a.b.a"), &[(u("."), u("$&$&"))]),
            u("a..b..a")
        );
        assert_eq!(simplify_ngrams(&u("aaaa"), &[(u("aa"), u("b"))]), u("bb"));
        let astral = u("A😀B");
        assert_eq!(simplify_ngrams(&astral, &[(vec![], u("-"))]), u("-A-😀-B-"));
        let malformed = [0xd83d, 0x306f, 0xde00];
        assert_eq!(normalize(&malformed, false, false), malformed);
        assert_eq!(
            collect_char_class(&malformed, CharClass::Nonword),
            vec![vec![0xd83d], vec![0xde00]]
        );
    }
}
