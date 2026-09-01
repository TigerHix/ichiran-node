use serde::Serialize;

use crate::morphology::Route;

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportStats {
    pub byte_length: usize,
    pub suffix_keys: usize,
    pub suffix_values: usize,
    pub suffix_forms: usize,
    pub suffix_conjugations: usize,
    pub suffix_classes: usize,
    pub counter_keys: usize,
    pub counter_variants: usize,
    pub digit_options: usize,
    pub list_members: usize,
    pub number_members: usize,
    pub splits: usize,
    pub split_parts: usize,
    pub hints: usize,
    pub collisions: usize,
    pub generated_rules: usize,
    pub generated_aliases: usize,
    pub strings: usize,
    pub string_bytes: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportConjugationProperty {
    pub pos: String,
    #[serde(rename = "type")]
    pub kind: u16,
    pub negative: Option<bool>,
    pub formal: Option<bool>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportConjugation {
    pub seq: u32,
    pub from: u32,
    pub via: Option<u32>,
    pub property: SupportConjugationProperty,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SupportConjugations {
    Root,
    Values(Vec<SupportConjugation>),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportSuffixForm {
    pub seq: u32,
    pub text: String,
    pub best_kanji: Option<String>,
    pub common_tags: String,
    pub ord: u16,
    pub common: Option<u8>,
    pub conjugatable: bool,
    pub nokanji: bool,
    pub conjugations: Option<SupportConjugations>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct SupportSuffixValue {
    pub keyword: String,
    pub form: Option<SupportSuffixForm>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct SupportSuffixMatch {
    pub start: usize,
    pub end: usize,
    pub text: String,
    pub values: Vec<SupportSuffixValue>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
pub enum SupportCounterClass {
    CounterText,
    NumberText,
    CounterHalfhour,
    CounterTsu,
    CounterHifumi,
    CounterDaysKun,
    CounterDaysOn,
    CounterMonths,
    CounterPeople,
    CounterWari,
    CounterAge,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub enum SupportDigit {
    #[serde(rename = ":off")]
    Off,
    Digit(i16),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct SupportDigitOption {
    pub digit: SupportDigit,
    pub values: Vec<String>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct SupportCounterSource {
    pub seq: u32,
    pub route: Route,
    pub text: String,
    pub ord: u16,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportCounterVariant {
    pub class_name: SupportCounterClass,
    pub text: String,
    pub kana: String,
    pub suffix: Option<String>,
    pub source: Option<SupportCounterSource>,
    pub ordinal: bool,
    pub foreign: bool,
    pub common: Option<u8>,
    pub suffix_descriptions: Vec<String>,
    pub digit_options: Vec<SupportDigitOption>,
    pub digit_set: Vec<u32>,
    pub allowed: Vec<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct SupportCounterMatch {
    pub start: usize,
    pub end: usize,
    pub text: String,
    pub values: Vec<SupportCounterVariant>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SupportSplitKind {
    Split,
    Segsplit,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportSplitConjugation {
    pub from: u32,
    pub via: bool,
    pub pos: String,
    #[serde(rename = "type")]
    pub kind: u16,
    pub negative: Option<bool>,
    pub formal: Option<bool>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportSplitWord {
    pub seq: u32,
    pub route: Route,
    pub text: String,
    pub best: Option<String>,
    pub ord: u16,
    pub common: Option<u8>,
    pub common_tags: String,
    pub conjugatable: bool,
    pub nokanji: bool,
    pub generated: Option<Vec<SupportSplitConjugation>>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum SupportSplitPart {
    Score,
    Pscore,
    Word(SupportSplitWord),
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportSplit {
    pub definition_seq: u32,
    pub route: Route,
    pub surface: String,
    pub kind: SupportSplitKind,
    pub parts: Vec<SupportSplitPart>,
    pub score: i32,
    pub primary: u8,
    pub connector: String,
    pub root: Vec<u32>,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SupportCollision {
    pub root_seq: u32,
    pub collision_seq: u32,
    pub via_seq: Option<u32>,
    pub route: Route,
    pub surface: String,
    pub rule_ids: Vec<u32>,
    pub n_kanji: u16,
    pub n_kana: u16,
    pub primary_nokanji: bool,
    pub archived: bool,
    pub prefer_kana: bool,
    pub prefer_kana_on_ordinal_zero: bool,
    pub pos: Vec<String>,
    pub skip_word: bool,
    pub final_particle: bool,
    pub semi_final_particle: bool,
    pub non_final_particle: bool,
    pub copula: bool,
    pub no_kanji_break_penalty: bool,
}
