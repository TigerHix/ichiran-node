//! Romanization over lossless JavaScript-compatible UTF-16 text.

use crate::characters::{
    MODIFIER_CHARACTERS, class_for_unit, is_kana_class, normalize as normalize_characters,
    simplify_ngrams, voice_char,
};

const HINT_MODIFIER: u16 = 0x200c;
const HINT_SPACE: u16 = 0x200b;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RomanizationName {
    HepburnBasic,
    HepburnSimple,
    HepburnPassport,
    HepburnTraditional,
    HepburnModified,
    KunreiSiki,
}

impl RomanizationName {
    pub const ALL: [Self; 6] = [
        Self::HepburnBasic,
        Self::HepburnSimple,
        Self::HepburnPassport,
        Self::HepburnTraditional,
        Self::HepburnModified,
        Self::KunreiSiki,
    ];

    pub fn as_str(self) -> &'static str {
        match self {
            Self::HepburnBasic => "hepburn-basic",
            Self::HepburnSimple => "hepburn-simple",
            Self::HepburnPassport => "hepburn-passport",
            Self::HepburnTraditional => "hepburn-traditional",
            Self::HepburnModified => "hepburn-modified",
            Self::KunreiSiki => "kunrei-siki",
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum KanaClass {
    Known(&'static str),
    Literal(Vec<u16>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum KanaTree {
    Atom(KanaClass),
    Modified {
        modifier: KanaClass,
        children: Vec<KanaTree>,
    },
    Empty,
}

pub trait RomanizationMethod {
    fn base(&self, item: &str) -> Vec<u16>;
    fn apply(&self, modifier: &str, tree: &[KanaTree]) -> Vec<u16>;
    fn simplify(&self, input: &[u16]) -> Vec<u16>;
    fn special(&self, input: &[u16]) -> Option<Vec<u16>>;
}

#[derive(Clone, Copy, Debug)]
pub struct BuiltinRomanization {
    name: RomanizationName,
}

impl BuiltinRomanization {
    pub const fn new(name: RomanizationName) -> Self {
        Self { name }
    }

    pub fn name(self) -> RomanizationName {
        self.name
    }
}

pub const HEPBURN_BASIC: BuiltinRomanization =
    BuiltinRomanization::new(RomanizationName::HepburnBasic);
pub const HEPBURN_SIMPLE: BuiltinRomanization =
    BuiltinRomanization::new(RomanizationName::HepburnSimple);
pub const HEPBURN_PASSPORT: BuiltinRomanization =
    BuiltinRomanization::new(RomanizationName::HepburnPassport);
pub const HEPBURN_TRADITIONAL: BuiltinRomanization =
    BuiltinRomanization::new(RomanizationName::HepburnTraditional);
pub const HEPBURN_MODIFIED: BuiltinRomanization =
    BuiltinRomanization::new(RomanizationName::HepburnModified);
pub const KUNREI_SIKI: BuiltinRomanization = BuiltinRomanization::new(RomanizationName::KunreiSiki);

pub fn romanization_method(name: RomanizationName) -> &'static BuiltinRomanization {
    match name {
        RomanizationName::HepburnBasic => &HEPBURN_BASIC,
        RomanizationName::HepburnSimple => &HEPBURN_SIMPLE,
        RomanizationName::HepburnPassport => &HEPBURN_PASSPORT,
        RomanizationName::HepburnTraditional => &HEPBURN_TRADITIONAL,
        RomanizationName::HepburnModified => &HEPBURN_MODIFIED,
        RomanizationName::KunreiSiki => &KUNREI_SIKI,
    }
}

fn u(value: &str) -> Vec<u16> {
    value.encode_utf16().collect()
}

pub fn process_hints(input: &[u16]) -> Vec<u16> {
    simplify_ngrams(
        input,
        &[
            (vec![HINT_SPACE], u(" ")),
            (vec![HINT_MODIFIER, 0x306f], u("わ")),
            (vec![HINT_MODIFIER, 0x30cf], u("ワ")),
            (vec![HINT_MODIFIER, 0x3078], u("え")),
            (vec![HINT_MODIFIER, 0x30d8], u("エ")),
            (vec![HINT_MODIFIER], vec![]),
        ],
    )
}

pub fn strip_hints(input: &[u16]) -> Vec<u16> {
    input
        .iter()
        .filter(|unit| !matches!(**unit, HINT_MODIFIER | HINT_SPACE))
        .copied()
        .collect()
}

pub fn get_character_classes(input: &[u16]) -> Vec<KanaClass> {
    let mut classes = Vec::new();
    let mut offset = 0;
    while offset < input.len() {
        let literal_class = match input[offset] {
            0x61 => Some("a"),
            0x69 => Some("i"),
            0x75 => Some("u"),
            0x65 => Some("e"),
            0x6f => Some("o"),
            0x6e => Some("n"),
            _ => None,
        };
        if let Some(class) = class_for_unit(input[offset]).or(literal_class) {
            classes.push(KanaClass::Known(class));
            offset += 1;
        } else {
            let width = if (0xd800..=0xdbff).contains(&input[offset])
                && input
                    .get(offset + 1)
                    .is_some_and(|unit| (0xdc00..=0xdfff).contains(unit))
            {
                2
            } else {
                1
            };
            classes.push(KanaClass::Literal(input[offset..offset + width].to_vec()));
            offset += width;
        }
    }
    classes
}

fn class_for_name(name: &str) -> Option<&'static str> {
    crate::characters::SOKUON_CHARACTERS
        .iter()
        .chain(crate::characters::ITERATION_CHARACTERS)
        .chain(MODIFIER_CHARACTERS)
        .chain(crate::characters::KANA_CHARACTERS)
        .find_map(|(class, _)| (*class == name).then_some(*class))
}

pub fn process_iteration_characters(classes: &[KanaClass]) -> Vec<KanaClass> {
    let mut output = Vec::new();
    let mut previous: Option<KanaClass> = None;
    for char_class in classes {
        match char_class {
            KanaClass::Known("iter") => {
                if let Some(previous) = &previous {
                    output.push(previous.clone());
                }
            }
            KanaClass::Known("iterV") => {
                if let Some(KanaClass::Known(previous)) = &previous {
                    let voiced = voice_char(previous);
                    output.push(KanaClass::Known(
                        class_for_name(&voiced).unwrap_or(previous),
                    ));
                } else if let Some(previous) = &previous {
                    output.push(previous.clone());
                }
            }
            _ => {
                output.push(char_class.clone());
                previous = Some(char_class.clone());
            }
        }
    }
    output
}

fn is_modifier(class: &KanaClass) -> bool {
    matches!(class, KanaClass::Known(name) if MODIFIER_CHARACTERS.iter().any(|(modifier, _)| modifier == name))
}

pub fn process_modifiers(classes: &[KanaClass]) -> Vec<KanaTree> {
    let mut output = Vec::new();
    let mut index = 0;
    while index < classes.len() {
        let char_class = classes[index].clone();
        if matches!(char_class, KanaClass::Known("sokuon")) {
            output.push(KanaTree::Modified {
                modifier: char_class,
                children: process_modifiers(&classes[index + 1..]),
            });
            break;
        }
        if is_modifier(&char_class) {
            let previous = output.pop().unwrap_or(KanaTree::Empty);
            output.push(KanaTree::Modified {
                modifier: char_class,
                children: vec![previous],
            });
        } else {
            output.push(KanaTree::Atom(char_class));
        }
        index += 1;
    }
    output
}

pub fn leftmost_atom(tree: &[KanaTree]) -> Option<&KanaClass> {
    match tree.first()? {
        KanaTree::Atom(class) => Some(class),
        KanaTree::Modified { children, .. } => leftmost_atom(children),
        KanaTree::Empty => None,
    }
}

pub fn romanize_core(method: &dyn RomanizationMethod, tree: &[KanaTree]) -> Vec<u16> {
    let mut output = Vec::new();
    for item in tree {
        match item {
            KanaTree::Empty => {}
            KanaTree::Atom(KanaClass::Literal(value)) => output.extend_from_slice(value),
            KanaTree::Atom(KanaClass::Known(class)) => {
                if is_kana_class(class) {
                    output.extend(method.base(class));
                } else {
                    output.extend(u(class));
                }
            }
            KanaTree::Modified {
                modifier: KanaClass::Known(modifier),
                children,
            } => {
                output.extend(method.apply(modifier, children));
            }
            KanaTree::Modified {
                modifier: KanaClass::Literal(modifier),
                children,
            } => {
                output.extend(romanize_core(method, children));
                output.extend_from_slice(modifier);
            }
        }
    }
    output
}

fn base_table(name: RomanizationName, item: &str) -> Option<&'static str> {
    let kunrei = name == RomanizationName::KunreiSiki;
    let modified = name == RomanizationName::HepburnModified;
    let special = match item {
        "shi" if kunrei => Some("si"),
        "chi" if kunrei => Some("ti"),
        "tsu" if kunrei => Some("tu"),
        "fu" if kunrei => Some("hu"),
        "wi" if kunrei => Some("i"),
        "we" if kunrei => Some("e"),
        "wo" if kunrei || modified => Some("o"),
        "ji" | "dji" if kunrei => Some("zi"),
        "dji" => Some("ji"),
        "dzu" => Some("zu"),
        _ => None,
    };
    if special.is_some() {
        return special;
    }
    Some(match item {
        "a" => "a",
        "i" => "i",
        "u" => "u",
        "e" => "e",
        "o" => "o",
        "ka" => "ka",
        "ki" => "ki",
        "ku" => "ku",
        "ke" => "ke",
        "ko" => "ko",
        "sa" => "sa",
        "shi" => "shi",
        "su" => "su",
        "se" => "se",
        "so" => "so",
        "ta" => "ta",
        "chi" => "chi",
        "tsu" => "tsu",
        "te" => "te",
        "to" => "to",
        "na" => "na",
        "ni" => "ni",
        "nu" => "nu",
        "ne" => "ne",
        "no" => "no",
        "ha" => "ha",
        "hi" => "hi",
        "fu" => "fu",
        "he" => "he",
        "ho" => "ho",
        "ma" => "ma",
        "mi" => "mi",
        "mu" => "mu",
        "me" => "me",
        "mo" => "mo",
        "ya" => "ya",
        "yu" => "yu",
        "yo" => "yo",
        "ra" => "ra",
        "ri" => "ri",
        "ru" => "ru",
        "re" => "re",
        "ro" => "ro",
        "wa" => "wa",
        "wi" => "wi",
        "we" => "we",
        "wo" => "wo",
        "n" => "n'",
        "ga" => "ga",
        "gi" => "gi",
        "gu" => "gu",
        "ge" => "ge",
        "go" => "go",
        "za" => "za",
        "ji" => "ji",
        "zu" => "zu",
        "ze" => "ze",
        "zo" => "zo",
        "da" => "da",
        "de" => "de",
        "do" => "do",
        "ba" => "ba",
        "bi" => "bi",
        "bu" => "bu",
        "be" => "be",
        "bo" => "bo",
        "pa" => "pa",
        "pi" => "pi",
        "pu" => "pu",
        "pe" => "pe",
        "po" => "po",
        "+a" => "a",
        "+i" => "i",
        "+u" => "u",
        "+e" => "e",
        "+o" => "o",
        "+ya" => "ya",
        "+yu" => "yu",
        "+yo" => "yo",
        "+wa" => "wa",
        "vu" => "vu",
        _ => return None,
    })
}

fn first_known(tree: &[KanaTree]) -> Option<&str> {
    match tree.first() {
        Some(KanaTree::Atom(KanaClass::Known(class))) => Some(class),
        _ => None,
    }
}

fn apply_default(modifier: &str, method: &dyn RomanizationMethod, tree: &[KanaTree]) -> Vec<u16> {
    let inner = romanize_core(method, tree);
    if modifier == "sokuon" {
        if inner.first().is_none_or(|first| *first > 127) {
            return inner;
        }
        let mut output = vec![inner[0]];
        output.extend(inner);
        return output;
    }
    if modifier == "longVowel" {
        return inner;
    }
    let mut output = inner;
    output.extend(modifier.to_ascii_lowercase().encode_utf16());
    output
}

fn generic_apply(method: &BuiltinRomanization, modifier: &str, tree: &[KanaTree]) -> Vec<u16> {
    let Some(yoon) = base_table(method.name, modifier) else {
        return apply_default(modifier, method, tree);
    };
    if first_known(tree) == Some("u") {
        return u(&format!("w{yoon}"));
    }
    if let Some(first @ ("a" | "i" | "e" | "o")) = first_known(tree) {
        return u(&format!(
            "{}{yoon}",
            base_table(method.name, first).unwrap_or("")
        ));
    }
    let mut inner = romanize_core(method, tree);
    inner.pop();
    inner.extend(u(yoon));
    inner
}

fn hepburn_apply(method: &BuiltinRomanization, modifier: &str, tree: &[KanaTree]) -> Vec<u16> {
    if modifier == "sokuon" && matches!(leftmost_atom(tree), Some(KanaClass::Known("chi"))) {
        let mut output = u("t");
        output.extend(romanize_core(method, tree));
        return output;
    }
    let special = match (modifier, first_known(tree)) {
        ("+ya", Some("shi")) => Some("sha"),
        ("+ya", Some("chi")) => Some("cha"),
        ("+ya", Some("ji" | "dji")) => Some("ja"),
        ("+yu", Some("shi")) => Some("shu"),
        ("+yu", Some("chi")) => Some("chu"),
        ("+yu", Some("ji" | "dji")) => Some("ju"),
        ("+yo", Some("shi")) => Some("sho"),
        ("+yo", Some("chi")) => Some("cho"),
        ("+yo", Some("ji" | "dji")) => Some("jo"),
        _ => None,
    };
    special.map_or_else(|| generic_apply(method, modifier, tree), u)
}

fn remove_unneeded_n_apostrophes(input: &[u16]) -> Vec<u16> {
    let mut output = Vec::new();
    let mut index = 0;
    while index < input.len() {
        if input[index..].starts_with(&[b'n' as u16, b'\'' as u16])
            && input
                .get(index + 2)
                .is_none_or(|unit| !matches!(*unit, 0x61 | 0x69 | 0x75 | 0x65 | 0x6f | 0x79))
        {
            output.push(b'n' as u16);
            if let Some(captured) = input.get(index + 2) {
                output.push(*captured);
                index += 3;
            } else {
                index += 2;
            }
        } else {
            output.push(input[index]);
            index += 1;
        }
    }
    output
}

fn replace_n_patterns(input: &[u16], hyphenate_vowels: bool, nasal_m: bool) -> Vec<u16> {
    let mut output = Vec::new();
    let mut index = 0;
    while index < input.len() {
        if hyphenate_vowels
            && input[index..].starts_with(&[b'n' as u16, b'\'' as u16])
            && input
                .get(index + 2)
                .is_some_and(|unit| matches!(*unit, 0x61 | 0x69 | 0x75 | 0x65 | 0x6f | 0x79))
        {
            output.extend([b'n' as u16, b'-' as u16, input[index + 2]]);
            index += 3;
        } else if nasal_m
            && input[index] == b'n' as u16
            && input
                .get(index + 1)
                .is_some_and(|unit| matches!(*unit, 0x6d | 0x62 | 0x70))
        {
            output.push(b'm' as u16);
            index += 1;
        } else {
            output.push(input[index]);
            index += 1;
        }
    }
    output
}

fn long_vowel_replacements(name: RomanizationName) -> &'static [(&'static str, &'static str)] {
    match name {
        RomanizationName::HepburnSimple => &[("oo", "o"), ("ou", "o"), ("uu", "u")],
        RomanizationName::HepburnPassport => &[("oo", "oh"), ("ou", "oh"), ("uu", "u")],
        RomanizationName::HepburnTraditional => &[("oo", "ō"), ("ou", "ō"), ("uu", "ū")],
        RomanizationName::HepburnModified => &[
            ("oo", "ō"),
            ("ou", "ō"),
            ("uu", "ū"),
            ("aa", "ā"),
            ("ee", "ē"),
        ],
        RomanizationName::KunreiSiki => &[("oo", "ô"), ("ou", "ô"), ("uu", "û")],
        RomanizationName::HepburnBasic => &[],
    }
}

impl RomanizationMethod for BuiltinRomanization {
    fn base(&self, item: &str) -> Vec<u16> {
        base_table(self.name, item).map_or_else(|| u(&item.to_lowercase()), u)
    }

    fn apply(&self, modifier: &str, tree: &[KanaTree]) -> Vec<u16> {
        if self.name == RomanizationName::KunreiSiki {
            generic_apply(self, modifier, tree)
        } else {
            hepburn_apply(self, modifier, tree)
        }
    }

    fn simplify(&self, input: &[u16]) -> Vec<u16> {
        let mut result = remove_unneeded_n_apostrophes(input);
        let replacements: Vec<(Vec<u16>, Vec<u16>)> = long_vowel_replacements(self.name)
            .iter()
            .map(|(from, to)| (u(from), u(to)))
            .collect();
        result = simplify_ngrams(&result, &replacements);
        if self.name == RomanizationName::HepburnTraditional {
            result = replace_n_patterns(&result, true, true);
        }
        result
    }

    fn special(&self, input: &[u16]) -> Option<Vec<u16>> {
        if input == [0x3063] {
            Some(u("!"))
        } else if input == [0x30fc] {
            Some(u("~"))
        } else {
            None
        }
    }
}

pub fn romanize_list(classes: &[KanaClass], method: &dyn RomanizationMethod) -> Vec<u16> {
    let tree = process_modifiers(&process_iteration_characters(classes));
    method.simplify(&romanize_core(method, &tree))
}

pub fn romanize_word(
    input: &[u16],
    method: RomanizationName,
    original_spelling: Option<&[u16]>,
    should_normalize: bool,
) -> Vec<u16> {
    romanize_word_with_method(
        input,
        romanization_method(method),
        original_spelling,
        should_normalize,
    )
}

pub fn romanize_word_with_method(
    input: &[u16],
    method: &dyn RomanizationMethod,
    original_spelling: Option<&[u16]>,
    should_normalize: bool,
) -> Vec<u16> {
    let word = if should_normalize {
        normalize_characters(input, false, false)
    } else {
        input.to_vec()
    };
    if let Some(special) = method.special(original_spelling.unwrap_or(&word)) {
        return special;
    }
    romanize_list(&get_character_classes(&process_hints(&word)), method)
}

fn javascript_whitespace(unit: u16) -> bool {
    matches!(unit, 0x0009..=0x000d | 0x0020 | 0x00a0 | 0x1680 | 0x2000..=0x200a
        | 0x2028 | 0x2029 | 0x202f | 0x205f | 0x3000 | 0xfeff)
}

pub fn join_romanized_parts(parts: &[Vec<u16>]) -> Vec<u16> {
    let mut output = Vec::new();
    let mut last_was_space = true;
    for part in parts {
        let Some(first) = part.first() else { continue };
        if !last_was_space && *first <= 0x7f && (*first as u8).is_ascii_alphanumeric() {
            output.push(b' ' as u16);
        }
        output.extend_from_slice(part);
        last_was_space = part.last().is_some_and(|unit| javascript_whitespace(*unit));
    }
    output
}

#[cfg(test)]
pub(crate) fn romanize(input: &str) -> String {
    let input: Vec<u16> = input.encode_utf16().collect();
    String::from_utf16_lossy(&romanize_word(
        &input,
        RomanizationName::HepburnTraditional,
        None,
        true,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn preserves_literal_astral_and_malformed_utf16() {
        for input in [vec![0xd83d, 0xde00], vec![0xd83d], vec![0xde00]] {
            assert_eq!(
                romanize_word(&input, RomanizationName::HepburnTraditional, None, true),
                input
            );
        }
    }

    #[test]
    fn romanizes_m1_words() {
        assert_eq!(romanize("ねこ"), "neko");
        assert_eq!(romanize("たべた"), "tabeta");
    }
}
