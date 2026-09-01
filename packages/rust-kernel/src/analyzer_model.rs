use crate::morphology::Route;

pub const SCORE_FLAG_STRONG: u8 = 1 << 0;
pub const SCORE_FLAG_PRIMARY: u8 = 1 << 1;
pub const SCORE_FLAG_COMMON: u8 = 1 << 2;
pub const SCORE_FLAG_LONG: u8 = 1 << 3;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ConjugationProperty {
    pub pos: String,
    pub kind: u8,
    pub negative: Option<bool>,
    pub formal: Option<bool>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Conjugation {
    pub seq: i64,
    pub from: i64,
    pub via: Option<i64>,
    pub property: ConjugationProperty,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct SequenceFacts {
    pub all_archived: bool,
    pub prefer_kana: bool,
    pub prefer_kana_on_ordinal_zero: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct EntryScoreFacts {
    pub root: bool,
    pub n_kanji: u32,
    pub primary_nokanji: bool,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ScoreModifier {
    pub multiplier: i32,
    pub constant: i32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ScoreWordKind {
    Word,
    Counter,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScoreSplit {
    Add(i32),
    Proportional(i32),
    Parts {
        score: i32,
        parts: Vec<ScoreCandidate>,
        truncated_last: Option<Box<ScoreCandidate>>,
    },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SuruBreakFacts {
    pub suffix_text: Vec<u16>,
    pub candidate: Box<ScoreCandidate>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct WordScoreFacts {
    pub kind: ScoreWordKind,
    pub text: Vec<u16>,
    pub true_text: Vec<u16>,
    pub true_text_follows_text: bool,
    pub route: Route,
    pub seq: Option<i64>,
    pub ord: i32,
    pub common: Option<i32>,
    pub nokanji: bool,
    pub entry: Option<EntryScoreFacts>,
    pub conjugation_only: bool,
    pub conjugations: Vec<Conjugation>,
    pub positions: Vec<String>,
    pub self_facts: SequenceFacts,
    pub lineage: SequenceFacts,
    pub inherited_common: Option<i32>,
    pub inherited_ord: Option<i32>,
    pub split: Option<ScoreSplit>,
    pub suru_break: Option<SuruBreakFacts>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CompoundScoreFacts {
    pub text: Vec<u16>,
    pub base: Box<ScoreCandidate>,
    pub modifier: ScoreModifier,
    pub conjugations: Vec<Conjugation>,
    pub suru_break: Option<SuruBreakFacts>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScoreCandidate {
    Word(WordScoreFacts),
    Compound(CompoundScoreFacts),
}

impl ScoreCandidate {
    pub fn text(&self) -> &[u16] {
        match self {
            Self::Word(word) => &word.text,
            Self::Compound(compound) => &compound.text,
        }
    }

    pub fn suru_break(&self) -> Option<&SuruBreakFacts> {
        match self {
            Self::Word(word) => word.suru_break.as_ref(),
            Self::Compound(compound) => compound.suru_break.as_ref(),
        }
    }

    pub fn with_text(&self, text: Vec<u16>) -> Self {
        match self {
            Self::Word(word) => {
                let mut word = word.clone();
                if word.true_text_follows_text {
                    word.true_text.clone_from(&text);
                }
                word.text = text;
                Self::Word(word)
            }
            Self::Compound(compound) => {
                let mut compound = compound.clone();
                compound.text = text;
                Self::Compound(compound)
            }
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ScoreOptions {
    pub final_word: bool,
    pub use_length: Option<i32>,
    pub modifier: ScoreModifier,
    pub kanji_break: Option<Vec<usize>>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SplitScoreInfo {
    Add(i32),
    Parts(Vec<i32>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScoreBreakdown {
    pub property_score: i32,
    pub kanji_break: Option<Vec<usize>>,
    pub use_length_bonus: i32,
    pub split: Option<SplitScoreInfo>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScoreInfo {
    pub positions: Vec<String>,
    pub seq_set: Vec<i64>,
    pub conjugations: Vec<Conjugation>,
    pub common: Option<i32>,
    pub breakdown: ScoreBreakdown,
    pub flags: u8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ScoreResult {
    pub score: i32,
    pub info: ScoreInfo,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuleWordKind {
    Simple,
    Proxy,
    Compound,
    Counter,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SegmentRuleFacts {
    pub text: Vec<u16>,
    pub word_kind: RuleWordKind,
    pub score_info: Option<ScoreInfo>,
    pub compound_end_seq: Option<i64>,
    pub compound_end_text: Option<Vec<u16>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Segment {
    pub candidate_id: i64,
    pub start: usize,
    pub end: usize,
    pub score: f64,
    pub common: Option<i32>,
    pub entity: bool,
    pub rules: Option<SegmentRuleFacts>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SegmentGroup {
    pub group_id: i64,
    pub start: usize,
    pub end: usize,
    pub segments: Vec<Segment>,
    pub matches: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathAdjustment {
    pub score: f64,
    pub start: usize,
    pub end: usize,
    pub description: String,
    pub connector: String,
}

#[derive(Clone, Debug, PartialEq)]
pub enum PathPart {
    Group(SegmentGroup),
    Adjustment(PathAdjustment),
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathTransition {
    pub right: SegmentGroup,
    pub adjustment: Option<PathAdjustment>,
    pub left: SegmentGroup,
}

#[derive(Clone, Debug, PartialEq)]
pub struct PathResult {
    pub score: f64,
    pub parts: Vec<PathPart>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EntityHint {
    pub start: usize,
    pub end: usize,
    pub boost: Option<f64>,
}

pub fn utf16(text: &str) -> Vec<u16> {
    text.encode_utf16().collect()
}
