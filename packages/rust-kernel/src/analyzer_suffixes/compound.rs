use crate::analyzer_lexicon::{
    CandidateComponent, CandidateKind, ConjugationSelection, MaterializedCandidate,
};
use crate::analyzer_model::{CompoundScoreFacts, ScoreCandidate, ScoreModifier, SuruBreakFacts};
use crate::characters::{CharClass, test_word};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SuffixCompound {
    pub stem: usize,
    pub connector: Vec<u16>,
    pub modifier: ScoreModifier,
    pub patch: Option<(Vec<u16>, Vec<u16>)>,
    pub suru_break: bool,
}

fn truncate_reading(
    reading: &[u16],
    stem: usize,
    patch: Option<&(Vec<u16>, Vec<u16>)>,
) -> Vec<u16> {
    let (remove, replacement) = patch.map_or((stem, &[][..]), |(remove, replacement)| {
        (remove.len(), replacement.as_slice())
    });
    let mut result = reading[..reading.len().saturating_sub(remove)].to_vec();
    result.extend_from_slice(replacement);
    result
}

fn component(candidate: &MaterializedCandidate, primary: bool) -> CandidateComponent {
    CandidateComponent {
        text: candidate.text.clone(),
        true_text: (candidate.true_text != candidate.text).then(|| candidate.true_text.clone()),
        route: candidate.route,
        reading: candidate.reading.clone(),
        entry_index: candidate.entry_index,
        root: candidate.root.clone(),
        inflection: candidate.inflection.clone(),
        primary,
        public_seq: candidate.public_seq,
        physical_key: candidate.physical_key.clone(),
        physical_group: candidate.physical_group,
        suffix_class: candidate.suffix_class.clone(),
        definition_seq: candidate.definition_seq,
        semantic_members: candidate.semantic_members.clone(),
        identity_roots: candidate.identity_roots.clone(),
        conjugation_selection: candidate.conjugation_selection,
    }
}

pub fn compound_suffix(
    primary: &MaterializedCandidate,
    suffix: &MaterializedCandidate,
    suffix_text: &[u16],
    surface: &[u16],
    options: &SuffixCompound,
    suffix_class: Option<String>,
) -> MaterializedCandidate {
    let mut reading = truncate_reading(&primary.reading, options.stem, options.patch.as_ref());
    reading.extend_from_slice(&options.connector);
    reading.extend_from_slice(suffix_text);

    let mut components = if primary.kind == CandidateKind::Compound {
        primary
            .components
            .iter()
            .cloned()
            .map(|mut value| {
                value.primary = value.physical_key == primary.physical_key;
                value
            })
            .collect()
    } else {
        vec![component(primary, true)]
    };
    components.push(component(
        suffix,
        suffix.physical_key == primary.physical_key,
    ));

    let (base, modifier, previous_suru_break) = match &primary.score_facts {
        ScoreCandidate::Compound(previous) => (
            (*previous.base).clone(),
            ScoreModifier {
                multiplier: previous.modifier.multiplier + options.modifier.multiplier,
                constant: previous.modifier.constant + options.modifier.constant,
            },
            previous.suru_break.clone(),
        ),
        value => (value.clone(), options.modifier, None),
    };
    let conjugations = match &suffix.score_facts {
        ScoreCandidate::Word(value) => value.conjugations.clone(),
        ScoreCandidate::Compound(value) => value.conjugations.clone(),
    };
    let suru_break = if options.suru_break {
        Some(SuruBreakFacts {
            suffix_text: suffix_text.to_vec(),
            candidate: Box::new(suffix.score_facts.clone()),
        })
    } else {
        previous_suru_break
    };
    let score_facts = ScoreCandidate::Compound(CompoundScoreFacts {
        text: surface.to_vec(),
        base: Box::new(base),
        modifier,
        conjugations,
        suru_break,
    });
    MaterializedCandidate {
        kind: CandidateKind::Compound,
        text: surface.to_vec(),
        true_text: surface.to_vec(),
        route: if test_word(surface, CharClass::Kana) {
            crate::morphology::Route::Kana
        } else {
            crate::morphology::Route::Kanji
        },
        reading,
        public_seq: primary.public_seq,
        physical_seq: primary.physical_seq,
        physical_key: primary.physical_key.clone(),
        physical_group: primary.physical_group,
        lookup_locators: Vec::new(),
        member_ord: primary.member_ord,
        entry_index: primary.entry_index,
        root: primary.root.clone(),
        inflection: suffix.inflection.clone(),
        score_facts,
        components,
        counter: None,
        suffix_class,
        definition_seq: primary.definition_seq,
        semantic_members: primary.semantic_members.clone(),
        identity_roots: primary.identity_roots.clone(),
        conjugation_selection: ConjugationSelection::Default,
    }
}

pub fn abbreviate_suffix(
    candidate: &MaterializedCandidate,
    root: &[u16],
    suffix: &[u16],
    surface: &[u16],
    stem: usize,
    patch: Option<&(Vec<u16>, Vec<u16>)>,
) -> MaterializedCandidate {
    let mut reading = truncate_reading(&candidate.reading, stem, patch);
    reading.extend_from_slice(suffix);
    let mut result = candidate.clone();
    result.reading = reading;
    if candidate.kind == CandidateKind::Compound {
        result.text = surface.to_vec();
        result.true_text = surface.to_vec();
        result.score_facts = candidate.score_facts.with_text(surface.to_vec());
        return result;
    }

    let mut text = root.to_vec();
    text.extend_from_slice(suffix);
    result.kind = CandidateKind::Proxy;
    result.text = text.clone();
    if let ScoreCandidate::Word(mut facts) = result.score_facts {
        facts.text = text;
        facts.true_text_follows_text = false;
        result.score_facts = ScoreCandidate::Word(facts);
    }
    result
}

pub fn unique_suffix(suffix_class: &str, matches: &[MaterializedCandidate]) -> bool {
    const ALWAYS: &[&str] = &[
        ":ra", ":nai-n", ":dewanai", ":eba", ":teba", ":reba", ":keba", ":geba", ":neba", ":beba",
        ":meba", ":seba", ":ii", ":mo", ":nikui", ":gai",
    ];
    if ALWAYS.contains(&suffix_class) {
        return true;
    }
    if suffix_class == ":sa" {
        return matches.iter().any(|candidate| {
            matches!(&candidate.score_facts, ScoreCandidate::Word(facts)
                if facts.entry.is_some_and(|entry| entry.root) && !facts.conjugation_only)
        });
    }
    if suffix_class == ":desu" {
        return !matches.iter().all(|candidate| {
            conjugations(&candidate.score_facts)
                .iter()
                .any(|conjugation| conjugation.from == 2_755_350)
        });
    }
    false
}

fn conjugations(candidate: &ScoreCandidate) -> &[crate::analyzer_model::Conjugation] {
    match candidate {
        ScoreCandidate::Word(value) => &value.conjugations,
        ScoreCandidate::Compound(value) => &value.conjugations,
    }
}
