use crate::analyzer_model::EntityHint;
use crate::characters::{BasicSplitType, basic_split};
use crate::error::{ErrorCode, KernelError, Result};

pub const MAX_ANALYZER_LIMIT: usize = 10;
pub const MAX_ANALYZER_TEXT_LENGTH: usize = 4096;
pub const MAX_ANALYZER_WORD_LENGTH: usize = 256;
pub const MAX_ANALYZER_ENTITIES: usize = 64;
pub const MAX_ANALYZER_ENTITY_ABS_BOOST: f64 = 1_000_000.0;

#[derive(Clone, Debug, PartialEq)]
pub struct AnalyzeOptions {
    pub limit: usize,
    pub entities: Vec<EntityHint>,
    pub normalize_punctuation: bool,
}

impl Default for AnalyzeOptions {
    fn default() -> Self {
        Self {
            limit: 5,
            entities: Vec::new(),
            normalize_punctuation: false,
        }
    }
}

pub fn validate(input: &[u16], options: &AnalyzeOptions) -> Result<()> {
    if !(1..=MAX_ANALYZER_LIMIT).contains(&options.limit) {
        return invalid(format!(
            "limit must be an integer from 1 to {MAX_ANALYZER_LIMIT}"
        ));
    }
    if input.len() > MAX_ANALYZER_TEXT_LENGTH {
        return invalid(format!(
            "text must contain at most {MAX_ANALYZER_TEXT_LENGTH} UTF-16 code units"
        ));
    }
    if basic_split(input).iter().any(|segment| {
        segment.kind == BasicSplitType::Word && segment.text.len() > MAX_ANALYZER_WORD_LENGTH
    }) {
        return invalid(format!(
            "each analyzable word must contain at most {MAX_ANALYZER_WORD_LENGTH} UTF-16 code units"
        ));
    }
    if options.entities.len() > MAX_ANALYZER_ENTITIES {
        return invalid(format!(
            "entities must contain at most {MAX_ANALYZER_ENTITIES} hints"
        ));
    }
    for (index, entity) in options.entities.iter().enumerate() {
        if entity.start >= entity.end || entity.end > input.len() {
            return invalid(format!(
                "entities[{index}] must be a non-empty span within the input"
            ));
        }
        if entity
            .boost
            .is_some_and(|boost| !boost.is_finite() || boost.abs() > MAX_ANALYZER_ENTITY_ABS_BOOST)
        {
            return invalid(format!(
                "entities[{index}].boost must be finite and between -{MAX_ANALYZER_ENTITY_ABS_BOOST} and {MAX_ANALYZER_ENTITY_ABS_BOOST}"
            ));
        }
    }
    Ok(())
}

fn invalid<T>(message: String) -> Result<T> {
    Err(KernelError::new(ErrorCode::InvalidInput, message))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn utf16(text: &str) -> Vec<u16> {
        text.encode_utf16().collect()
    }

    #[test]
    fn validates_the_public_typescript_bounds_in_utf16_units() {
        let input = utf16("猫😀");
        assert!(validate(&input, &AnalyzeOptions::default()).is_ok());
        assert_eq!(input.len(), 3);

        let invalid_limit = AnalyzeOptions {
            limit: 11,
            ..AnalyzeOptions::default()
        };
        assert_eq!(
            validate(&input, &invalid_limit).unwrap_err().message,
            "limit must be an integer from 1 to 10"
        );

        let invalid_entity = AnalyzeOptions {
            entities: vec![EntityHint {
                start: 1,
                end: 4,
                boost: None,
            }],
            ..AnalyzeOptions::default()
        };
        assert_eq!(
            validate(&input, &invalid_entity).unwrap_err().message,
            "entities[0] must be a non-empty span within the input"
        );
    }

    #[test]
    fn rejects_nonfinite_and_oversized_entity_boosts() {
        for boost in [f64::NAN, f64::INFINITY, 1_000_000.5] {
            let options = AnalyzeOptions {
                entities: vec![EntityHint {
                    start: 0,
                    end: 1,
                    boost: Some(boost),
                }],
                ..AnalyzeOptions::default()
            };
            assert_eq!(
                validate(&utf16("猫"), &options).unwrap_err().message,
                "entities[0].boost must be finite and between -1000000 and 1000000"
            );
        }
    }

    #[test]
    fn limits_each_word_but_allows_long_misc_chunks() {
        let long_word = vec![0x732b; MAX_ANALYZER_WORD_LENGTH + 1];
        assert_eq!(
            validate(&long_word, &AnalyzeOptions::default())
                .unwrap_err()
                .message,
            "each analyzable word must contain at most 256 UTF-16 code units"
        );

        let long_misc = vec![b' ' as u16; MAX_ANALYZER_WORD_LENGTH + 1];
        assert!(validate(&long_misc, &AnalyzeOptions::default()).is_ok());
    }
}
