use std::collections::HashSet;

use super::LegacySense;
use super::detailed::LegacyContext;
use crate::characters::as_hiragana;
use crate::details::{DetailEntry, DetailForm, DetailSense};
use crate::error::{ErrorCode, KernelError, Result};
use crate::morphology::Route;

const PRESENTED_PROPERTY_TAGS: &[&str] = &["field", "pos", "s_inf", "stagk", "stagr"];

struct ReadingRestriction {
    reading: String,
    written: String,
}

pub(super) fn senses(
    entry: &DetailEntry,
    entry_index: usize,
    context: &LegacyContext<'_>,
    route: Route,
    form: &str,
    reading: &str,
    pos_filter: Option<&[String]>,
) -> Result<Vec<LegacySense>> {
    let restrictions = entry_restrictions(context, entry_index)?;
    let reversed = final_property_group(entry);
    let mut ordered = entry.senses.iter().collect::<Vec<_>>();
    ordered.sort_by_key(|sense| sense.ord);
    let mut carried_pos = Vec::new();
    let mut result = Vec::new();
    for sense in ordered {
        let values = |tag: &str| {
            let mut found = properties(sense, tag);
            if reversed == Some((sense.ord, tag)) {
                found.reverse();
            }
            found
        };
        let pos = values("pos");
        if !pos.is_empty() {
            carried_pos = pos;
        }
        if pos_filter
            .is_some_and(|filter| !carried_pos.iter().any(|position| filter.contains(position)))
            || !sense_allowed(sense, entry, route, form, reading, &restrictions)?
        {
            continue;
        }
        let mut glosses = sense.glosses.iter().collect::<Vec<_>>();
        glosses.sort_by_key(|gloss| gloss.ord);
        let field = values("field");
        let info = values("s_inf");
        result.push(LegacySense {
            pos: format!("[{}]", carried_pos.join(",")),
            gloss: glosses
                .into_iter()
                .map(|gloss| gloss.text.as_str())
                .collect::<Vec<_>>()
                .join("; "),
            field: (!field.is_empty()).then(|| format!("{{{}}}", field.join(","))),
            info: (!info.is_empty()).then(|| info.join("; ")),
        });
    }
    Ok(result)
}

fn properties(sense: &DetailSense, tag: &str) -> Vec<String> {
    let mut values = sense
        .properties
        .iter()
        .filter(|property| property.tag == tag)
        .collect::<Vec<_>>();
    values.sort_by_key(|property| property.ord);
    values
        .into_iter()
        .map(|property| property.text.clone())
        .collect()
}

fn final_property_group(entry: &DetailEntry) -> Option<(u32, &str)> {
    let mut senses = entry.senses.iter().collect::<Vec<_>>();
    senses.sort_by_key(|sense| sense.ord);
    let mut result = None;
    for sense in senses {
        let mut tags = sense
            .properties
            .iter()
            .filter(|property| PRESENTED_PROPERTY_TAGS.contains(&property.tag))
            .map(|property| property.tag)
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();
        tags.sort_unstable();
        if let Some(tag) = tags.last() {
            result = Some((sense.ord, *tag));
        }
    }
    result
}

fn entry_restrictions(
    context: &LegacyContext<'_>,
    entry_index: usize,
) -> Result<Vec<ReadingRestriction>> {
    let start = context.roots.restriction_start(entry_index)?;
    let end = context.roots.restriction_end(entry_index)?;
    (start..end)
        .map(|index| {
            Ok(ReadingRestriction {
                reading: resolve_reference(
                    context,
                    context.roots.restriction_reading_reference(index)?,
                )?,
                written: resolve_reference(
                    context,
                    context.roots.restriction_written_reference(index)?,
                )?,
            })
        })
        .collect()
}

fn resolve_reference(context: &LegacyContext<'_>, reference: u32) -> Result<String> {
    context
        .roots
        .resolve_surface_reference(reference, |rank| context.surface.direct_surface(rank))?
        .ok_or_else(|| {
            KernelError::new(
                ErrorCode::CorruptPayload,
                "legacy restriction contains an empty surface reference",
            )
        })
}

fn sense_allowed(
    sense: &DetailSense,
    entry: &DetailEntry,
    route: Route,
    form: &str,
    reading: &str,
    restrictions: &[ReadingRestriction],
) -> Result<bool> {
    let stagk = properties(sense, "stagk");
    let stagr = properties(sense, "stagr");
    if stagk.is_empty() && stagr.is_empty() {
        return Ok(true);
    }
    let current = if route == Route::Kanji { form } else { reading };
    if stagk.iter().any(|value| value == current) || stagr.iter().any(|value| value == current) {
        return Ok(true);
    }
    if route == Route::Kana {
        let current_hiragana = hiragana(current, "legacy sense reading")?;
        if stagr.iter().any(|value| value == &current_hiragana) {
            return Ok(true);
        }
    }
    if (route == Route::Kanji && stagr.is_empty()) || (route == Route::Kana && stagk.is_empty()) {
        return Ok(false);
    }
    if route == Route::Kanji {
        return Ok(entry.forms.iter().any(|value| {
            value.route == Route::Kana
                && stagr.contains(&value.text)
                && reading_matches_written(value, current, restrictions)
        }));
    }
    let current_readings = entry
        .forms
        .iter()
        .filter(|value| value.route == Route::Kana && value.text == current);
    Ok(current_readings.into_iter().any(|current_reading| {
        entry.forms.iter().any(|value| {
            value.route == Route::Kanji
                && stagk.contains(&value.text)
                && reading_matches_written(current_reading, &value.text, restrictions)
        })
    }))
}

fn reading_matches_written(
    reading: &DetailForm,
    written: &str,
    restrictions: &[ReadingRestriction],
) -> bool {
    if reading.nokanji {
        return false;
    }
    let restricted = restrictions
        .iter()
        .filter(|value| value.reading == reading.text)
        .map(|value| value.written.as_str())
        .collect::<Vec<_>>();
    restricted.is_empty() || restricted.contains(&written)
}

fn hiragana(value: &str, label: &str) -> Result<String> {
    String::from_utf16(&as_hiragana(&value.encode_utf16().collect::<Vec<_>>())).map_err(|_| {
        KernelError::new(
            ErrorCode::Internal,
            format!("{label} produced malformed UTF-16"),
        )
    })
}
