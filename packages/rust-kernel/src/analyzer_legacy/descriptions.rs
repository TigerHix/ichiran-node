pub(super) fn conjugation(kind: u8) -> String {
    match kind {
        1 => "Non-past",
        2 => "Past (~ta)",
        3 => "Conjunctive (~te)",
        4 => "Provisional (~eba)",
        5 => "Potential",
        6 => "Passive",
        7 => "Causative",
        8 => "Causative-Passive",
        9 => "Volitional",
        10 => "Imperative",
        11 => "Conditional (~tara)",
        12 => "Alternative (~tari)",
        13 => "Continuative (~i)",
        50 => "Adverbial",
        51 => "Adjective Stem",
        52 => "Negative Stem",
        53 => "Causative (~su)",
        54 => "Old/literary form",
        value => return value.to_string(),
    }
    .to_owned()
}

pub(super) fn suffix(class: &str) -> Option<&'static str> {
    Some(match class {
        ":chau" => "indicates completion (to finish ...)",
        ":ha" => "topic marker particle",
        ":tai" => "want to... / would like to...",
        ":iru" => "indicates continuing action (to be ...ing)",
        ":oru" => "indicates continuing action (to be ...ing) (humble)",
        ":aru" => "indicates completion / finished action",
        ":kuru" => "indicates action that had been continuing up till now / came to be ",
        ":oku" => "to do in advance / to leave in the current state expecting a later change",
        ":kureru" => "(asking) to do something for one",
        ":morau" => "(asking) to get somebody to do something",
        ":itadaku" => "(asking) to get somebody to do something (polite)",
        ":iku" => "is becoming / action starting now and continuing",
        ":suru" => "makes a verb from a noun",
        ":itasu" => "makes a verb from a noun (humble)",
        ":sareru" => "makes a verb from a noun (honorific or passive)",
        ":saseru" => "let/make someone/something do ...",
        ":rou" => "probably / it seems that... / I guess ...",
        ":ii" => "it's ok if ... / is it ok if ...?",
        ":mo" => "even if ...",
        ":sugiru" => "to be too (much) ...",
        ":nikui" => "difficult to...",
        ":gatai" => "difficult to...",
        ":sa" => "-ness (degree or condition of adjective)",
        ":tsutsu" => "while ... / in the process of ...",
        ":tsutsuaru" => "to be doing ... / to be in the process of doing ...",
        ":uru" => "can ... / to be able to ...",
        ":sou" => "looking like ... / seeming ...",
        ":nai" => "negative suffix",
        ":ra" => "pluralizing suffix (not polite)",
        ":kudasai" => "please do ...",
        ":yagaru" => "indicates disdain or contempt",
        ":naru" => "to become ...",
        ":desu" => "formal copula",
        ":desho" => "it seems/perhaps/don't you think?",
        ":tosuru" => "to try to .../to be about to...",
        ":garu" => "to feel .../have a ... impression of someone",
        ":me" => "somewhat/-ish",
        ":gai" => "worth it to ...",
        ":tasou" => "seem to want to... (tai+sou)",
        "2826528" => "polite prefix",
        "2028980" => "at / in / by",
        "2028970" => "or / questioning particle",
        "2028990" => "to / at / in",
        "2029010" => "indicates direct object of action",
        "1469800" => "indicates possessive (...'s)",
        "2086960" => "quoting particle",
        "1002980" => "from / because",
        _ => return None,
    })
}

/// Maps analyzer-internal suffix classes to stable, locale-independent product IDs.
pub(super) fn suffix_id(class: &str) -> Option<&'static str> {
    Some(match class {
        ":chau" => "chau",
        ":ha" => "ha",
        ":tai" => "tai",
        ":iru" => "iru",
        ":oru" => "oru",
        ":aru" => "aru",
        ":kuru" => "kuru",
        ":oku" => "oku",
        ":kureru" => "kureru",
        ":morau" => "morau",
        ":itadaku" => "itadaku",
        ":iku" => "iku",
        ":suru" => "suru",
        ":itasu" => "itasu",
        ":sareru" => "sareru",
        ":saseru" => "saseru",
        ":rou" => "rou",
        ":ii" => "ii",
        ":mo" => "mo",
        ":sugiru" => "sugiru",
        ":nikui" => "nikui",
        ":gatai" => "gatai",
        ":sa" => "sa",
        ":tsutsu" => "tsutsu",
        ":tsutsuaru" => "tsutsuaru",
        ":uru" => "uru",
        ":sou" => "sou",
        ":nai" => "nai",
        ":ra" => "ra",
        ":kudasai" => "kudasai",
        ":yagaru" => "yagaru",
        ":naru" => "naru",
        ":desu" => "desu",
        ":desho" => "desho",
        ":tosuru" => "tosuru",
        ":garu" => "garu",
        ":me" => "me",
        ":gai" => "gai",
        ":tasou" => "tasou",
        "2826528" => "polite-prefix",
        "2028980" => "particle-ni",
        "2028970" => "particle-ka",
        "2028990" => "particle-e",
        "2029010" => "particle-o",
        "1469800" => "particle-no",
        "2086960" => "particle-to",
        "1002980" => "particle-kara",
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn retained_description_tables_cover_named_and_sequence_suffixes() {
        assert_eq!(conjugation(13), "Continuative (~i)");
        assert_eq!(conjugation(99), "99");
        assert_eq!(suffix(":tai"), Some("want to... / would like to..."));
        assert_eq!(suffix("2029010"), Some("indicates direct object of action"));
        assert_eq!(suffix(":missing"), None);
        assert_eq!(suffix_id(":tai"), Some("tai"));
        assert_eq!(suffix_id("2029010"), Some("particle-o"));
        assert_eq!(suffix_id(":missing"), None);
    }
}
