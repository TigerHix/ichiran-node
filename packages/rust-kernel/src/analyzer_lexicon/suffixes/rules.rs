use super::super::*;
use crate::analyzer_model::ScoreModifier;
use crate::analyzer_suffixes::{SuffixCompound, abbreviate_suffix, compound_suffix};
use crate::characters::as_hiragana;
use crate::support::SupportSuffixForm;

impl AnalyzerLexicon<'_> {
    #[allow(clippy::too_many_arguments)]
    pub fn apply_suffix(
        &mut self,
        keyword: &str,
        root: &[u16],
        suffix: &[u16],
        form: Option<&SupportSuffixForm>,
        surface: &[u16],
        depth: usize,
    ) -> Result<Vec<MaterializedCandidate>> {
        let mut options = SuffixCompound::default();
        let mut primary = Vec::new();
        let next = depth + 1;

        match keyword {
            ":tai" => {
                if root != u("い") {
                    primary = self.with_types(root, &[13], next)?;
                }
                options.modifier = modifier(5, 0);
            }
            ":ren" => {
                primary = self.with_types(root, &[13], next)?;
                options.modifier = modifier(5, 0);
            }
            ":ren-" => primary = self.with_types(root, &[13], next)?,
            ":neg" => {
                primary = self.with_types(root, &[13, 52], next)?;
                options.modifier = modifier(5, 0);
            }
            ":te" => primary = self.te(root, next)?,
            ":teiru" => {
                if root != u("いて") {
                    primary = self.te(root, next)?;
                }
                options.modifier = modifier(3, 0);
            }
            ":teiru+" => {
                if root != u("いて") {
                    primary = self.te(root, next)?;
                }
                options.modifier = modifier(6, 0);
            }
            ":te+space" => {
                primary = self.te(root, next)?;
                options.connector = u(" ");
                options.modifier = modifier(3, 0);
            }
            ":kudasai" => {
                primary = self.te(root, next)?;
                options.connector = u(" ");
                options.modifier = modifier(0, 360);
            }
            ":teren" => {
                if root != u("で") {
                    if ends(root, "て") || ends(root, "で") {
                        primary = self.with_types(root, &[3], next)?;
                    } else if root != u("い") {
                        primary = self.with_types(root, &[13], next)?;
                    }
                }
                options.modifier = modifier(4, 0);
            }
            ":teii" => {
                if ends(root, "て") || ends(root, "で") {
                    primary = self.with_types(root, &[3], next)?;
                }
                options.connector = u(" ");
                options.modifier = modifier(1, 0);
            }
            ":chau" => {
                options.stem = 1;
                let restored = if starts(suffix, "じ") {
                    Some("で")
                } else if starts(suffix, "ち") {
                    Some("て")
                } else {
                    None
                };
                if let Some(restored) = restored {
                    primary = self.with_types(&joined(root, restored), &[3], next)?;
                }
                options.modifier = modifier(5, 0);
            }
            ":to" => {
                options.stem = 1;
                let restored = if starts(suffix, "と") {
                    Some("て")
                } else if starts(suffix, "ど") {
                    Some("で")
                } else {
                    None
                };
                if let Some(restored) = restored {
                    primary = self.with_types(&joined(root, restored), &[3], next)?;
                }
            }
            ":suru" => {
                primary = self.with_pos(root, &["vs"])?;
                options.connector = u(" ");
                options.modifier = modifier(5, 0);
                options.suru_break = true;
            }
            ":sou" | ":sou+" => {
                if ends(root, "なさ") {
                    options.patch = Some((u("い"), u("さ")));
                    let modified = replace_last(root, "い");
                    primary = self.with_property(
                        &modified,
                        |value| value.property.negative != Some(false),
                        false,
                        next,
                    )?;
                } else if !["な", "よ", "よさ", "に", "き"]
                    .iter()
                    .any(|value| root == u(value))
                {
                    primary = self.with_types(root, &[13, 51, 50], next)?;
                }
                options.modifier = if keyword == ":sou+" {
                    modifier(1, 0)
                } else {
                    modifier(
                        0,
                        if root == u("から") {
                            40
                        } else if root == u("い") {
                            0
                        } else if root == u("出来") {
                            100
                        } else {
                            70
                        },
                    )
                };
            }
            ":rou" => {
                primary = self.with_types(root, &[2], next)?;
                options.modifier = modifier(1, 0);
            }
            ":adv" => {
                primary = self.with_types(root, &[50], next)?;
                options.modifier = modifier(1, 0);
            }
            ":sugiru" => {
                options.stem = 1;
                if root != u("い") {
                    if ends(root, "なさ") || ends(root, "無さ") {
                        options.patch = Some((u("い"), u("さ")));
                        let modified = replace_last(root, "い");
                        primary = if modified.len() > 2 {
                            self.with_property(
                                &modified,
                                |value| value.property.negative != Some(false),
                                false,
                                next,
                            )?
                        } else {
                            self.with_pos(&modified, &["adj-i"])?
                        };
                    } else {
                        primary = self.with_pos(&joined(root, "い"), &["adj-i"])?;
                    }
                }
                options.modifier = modifier(5, 0);
            }
            ":sa" => {
                primary = self.with_types(root, &[51], next)?;
                primary.extend(self.with_pos(root, &["adj-na"])?);
                options.modifier = modifier(2, 0);
            }
            ":iadj" => {
                primary = self.with_types(root, &[51], next)?;
                options.modifier = modifier(1, 0);
            }
            ":garu" => {
                if !["な", "い", "よ"].iter().any(|value| root == u(value)) {
                    primary = self.with_types(root, &[51], next)?;
                    if primary.is_empty() && ends(root, "そ") {
                        options.patch = Some((u("う"), Vec::new()));
                        primary = self
                            .full_at(&joined(root, "う"), next)?
                            .into_iter()
                            .filter(|value| {
                                value.kind == CandidateKind::Compound
                                    && value.suffix_class.as_deref() == Some(":sou")
                            })
                            .collect();
                    }
                }
            }
            ":ra" => {
                if !ends(root, "ら") {
                    primary = self.with_pos(root, &["pn"])?;
                    if primary.is_empty() {
                        primary = self.with_pos(&as_hiragana(root), &["pn"])?;
                    }
                    if primary.is_empty() {
                        primary = self.with_seq(root, &[1_580_640])?;
                    }
                }
                options.modifier = modifier(1, 0);
            }
            ":rashii" => {
                primary = self.with_types(root, &[2], next)?;
                primary.extend(self.with_types(&joined(root, "ら"), &[11], next)?);
                primary = dedupe(primary)?;
                options.modifier = modifier(3, 0);
            }
            ":desu" => {
                if ends(root, "ない") || ends(root, "なかった") {
                    primary = self.with_property(
                        root,
                        |value| value.property.negative != Some(false),
                        false,
                        next,
                    )?;
                }
                options.connector = u(" ");
                options.modifier = modifier(0, 200);
            }
            ":desho" => {
                if ends(root, "ない") {
                    primary = self.with_property(
                        root,
                        |value| value.property.negative != Some(false),
                        false,
                        next,
                    )?;
                }
                options.connector = u(" ");
                options.modifier = modifier(0, 300);
            }
            ":tosuru" => {
                primary = self.with_types(root, &[9], next)?;
                options.connector = u(" ");
                options.modifier = modifier(3, 0);
            }
            ":kurai" => {
                primary = self.with_types(root, &[2], next)?;
                options.connector = u(" ");
                options.modifier = modifier(3, 0);
            }
            ":nai" => {
                primary = self.with_property(
                    &joined(root, "ない"),
                    |value| {
                        value.from != 1_577_980
                            && value.from != 1_547_720
                            && value.property.negative != Some(false)
                    },
                    true,
                    next,
                )?;
                return Ok(abbreviations(primary, root, suffix, surface, 2, None));
            }
            ":nai-x" => {
                if root == u("せ") {
                    options.patch = Some((u("しない"), u("せ")));
                    primary = self.conj_of(&u("しない"), &[1_157_170])?;
                } else {
                    primary = self.with_property(
                        &joined(root, "ない"),
                        |value| value.from != 1_157_170 && value.property.negative != Some(false),
                        false,
                        next,
                    )?;
                }
                return Ok(abbreviations(
                    primary,
                    root,
                    suffix,
                    surface,
                    2,
                    options.patch.as_ref(),
                ));
            }
            ":nai-n" => {
                primary = self.with_property(
                    &joined(root, "ない"),
                    |value| {
                        value.from != 1_577_980
                            && value.from != 1_547_720
                            && value.property.negative != Some(false)
                    },
                    false,
                    next,
                )?;
                return Ok(abbreviations(primary, root, suffix, surface, 2, None));
            }
            ":nakereba" => {
                primary = self.full_at(&joined(root, "なければ"), next)?;
                return Ok(abbreviations(primary, root, suffix, surface, 4, None));
            }
            ":shimashou" => {
                primary = self.full_at(&joined(root, "しましょう"), next)?;
                return Ok(abbreviations(primary, root, suffix, surface, 5, None));
            }
            ":dewanai" => {
                primary = self.full_at(&joined(root, "ではない"), next)?;
                return Ok(abbreviations(primary, root, suffix, surface, 4, None));
            }
            ":teba" | ":reba" | ":keba" | ":geba" | ":neba" | ":beba" | ":meba" | ":seba" => {
                let restored = match keyword {
                    ":teba" => "てば",
                    ":reba" => "れば",
                    ":keba" => "けば",
                    ":geba" => "げば",
                    ":neba" => "ねば",
                    ":beba" => "べば",
                    ":meba" => "めば",
                    _ => "せば",
                };
                primary = self.full_at(&joined(root, restored), next)?;
                return Ok(abbreviations(primary, root, suffix, surface, 2, None));
            }
            ":ii" => {
                primary = self.full_at(&joined(root, "いい"), next)?;
                return Ok(abbreviations(primary, root, suffix, surface, 2, None));
            }
            _ => return Ok(Vec::new()),
        }

        let Some(form) = form else {
            return Ok(Vec::new());
        };
        let Some(suffix_candidate) = self.suffix_component(form)? else {
            return Ok(Vec::new());
        };
        // TypeScript uses `physicalSeq ?? publicSeq`: a generated negative
        // physical identity is authoritative and cannot fall back to publicSeq.
        let suffix_class = match suffix_candidate.physical_seq {
            Some(value) => match u32::try_from(value) {
                Ok(seq) => self.support.suffix_class(seq)?,
                Err(_) => None,
            },
            None => self
                .support
                .suffix_class(suffix_candidate.public_seq.unwrap_or(0))?,
        };
        Ok(primary
            .iter()
            .map(|value| {
                compound_suffix(
                    value,
                    &suffix_candidate,
                    suffix,
                    surface,
                    &options,
                    suffix_class.clone(),
                )
            })
            .collect())
    }

    fn te(&mut self, root: &[u16], depth: usize) -> Result<Vec<MaterializedCandidate>> {
        if root != u("で") && (ends(root, "て") || ends(root, "で")) {
            self.with_types(root, &[3], depth)
        } else {
            Ok(Vec::new())
        }
    }

    fn with_types(
        &mut self,
        surface: &[u16],
        types: &[u8],
        depth: usize,
    ) -> Result<Vec<MaterializedCandidate>> {
        let values = self.full_at(surface, depth)?;
        let selected = values
            .iter()
            .map(|value| {
                self.select_conjugations(
                    value,
                    |conjugation| types.contains(&conjugation.property.kind),
                    false,
                )
            })
            .collect::<Result<Vec<_>>>()?;
        Ok(selected.into_iter().flatten().collect())
    }

    fn with_property(
        &mut self,
        surface: &[u16],
        predicate: impl Fn(&Conjugation) -> bool,
        allow_root: bool,
        depth: usize,
    ) -> Result<Vec<MaterializedCandidate>> {
        let values = self.full_at(surface, depth)?;
        let selected = values
            .iter()
            .map(|value| self.select_conjugations(value, &predicate, allow_root))
            .collect::<Result<Vec<_>>>()?;
        Ok(selected.into_iter().flatten().collect())
    }

    fn with_pos(&mut self, surface: &[u16], wanted: &[&str]) -> Result<Vec<MaterializedCandidate>> {
        Ok(self
            .lexical(surface)?
            .into_iter()
            .filter(|value| {
                matches!(&value.score_facts, ScoreCandidate::Word(facts)
                    if value.kind == CandidateKind::Simple
                        && facts.entry.is_some_and(|entry| entry.root)
                        && !facts.conjugation_only
                        && wanted.iter().any(|pos| facts.positions.iter().any(|value| value == pos)))
            })
            .collect())
    }

    fn with_seq(&mut self, surface: &[u16], seqs: &[i64]) -> Result<Vec<MaterializedCandidate>> {
        Ok(self
            .lexical(surface)?
            .into_iter()
            .filter(|value| {
                matches!(&value.score_facts, ScoreCandidate::Word(facts)
                    if value.kind == CandidateKind::Simple
                        && facts.entry.is_some_and(|entry| entry.root)
                        && !facts.conjugation_only
                        && value.physical_seq.is_some_and(|seq| seqs.contains(&seq)))
            })
            .collect())
    }

    fn conj_of(&mut self, surface: &[u16], seqs: &[i64]) -> Result<Vec<MaterializedCandidate>> {
        Ok(self
            .lexical(surface)?
            .into_iter()
            .filter(|value| {
                value.physical_seq.is_some_and(|seq| seqs.contains(&seq))
                    || score_conjugations(&value.score_facts)
                        .iter()
                        .any(|conjugation| seqs.contains(&conjugation.from))
            })
            .collect())
    }
}

fn abbreviations(
    values: Vec<MaterializedCandidate>,
    root: &[u16],
    suffix: &[u16],
    surface: &[u16],
    stem: usize,
    patch: Option<&(Vec<u16>, Vec<u16>)>,
) -> Vec<MaterializedCandidate> {
    values
        .iter()
        .map(|value| abbreviate_suffix(value, root, suffix, surface, stem, patch))
        .collect()
}

fn score_conjugations(candidate: &ScoreCandidate) -> &[Conjugation] {
    match candidate {
        ScoreCandidate::Word(value) => &value.conjugations,
        ScoreCandidate::Compound(value) => &value.conjugations,
    }
}

fn modifier(multiplier: i32, constant: i32) -> ScoreModifier {
    ScoreModifier {
        multiplier,
        constant,
    }
}

fn u(value: &str) -> Vec<u16> {
    utf16(value)
}

fn starts(value: &[u16], prefix: &str) -> bool {
    value.starts_with(&u(prefix))
}

fn ends(value: &[u16], suffix: &str) -> bool {
    value.ends_with(&u(suffix))
}

fn joined(value: &[u16], suffix: &str) -> Vec<u16> {
    let mut result = value.to_vec();
    result.extend(u(suffix));
    result
}

fn replace_last(value: &[u16], suffix: &str) -> Vec<u16> {
    let mut result = value[..value.len().saturating_sub(1)].to_vec();
    result.extend(u(suffix));
    result
}
