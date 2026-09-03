use std::cmp::Ordering;
use std::collections::HashSet;

use super::descriptions;
use super::detailed::{Attempt, LegacyContext, LegacyDetailedSession};
use super::senses::senses;
use super::{
    LegacyConjugation, LegacyConjugationFlags, LegacyConjugationInfo, LegacyConjugationProperty,
};
use crate::characters::{CharClass, test_word};
use crate::details::DetailStore;
use crate::dto::{AnalysisRoot, LegacyConjugationSelection, LegacySemanticMember};
use crate::morphology::{MorphologyProperty, Route};

#[derive(Clone)]
struct StageItem<'a> {
    member: &'a LegacySemanticMember,
    depth: usize,
    order: usize,
}

struct StageRow<'a> {
    items: Vec<StageItem<'a>>,
    properties: Vec<MorphologyProperty>,
    via: bool,
    member_ord: Option<u8>,
    order: usize,
}

#[derive(Eq, PartialEq)]
enum StageIdentity {
    Group(u32, u8),
    Semantic(String, usize, u8),
    Unique(usize, usize),
}

#[derive(Eq, PartialEq)]
enum PropertyIdentity {
    Ord(u16),
    Value(MorphologyProperty),
}

pub(super) fn conjugation_forest(
    members: &[LegacySemanticMember],
    session: &mut LegacyDetailedSession,
    details: &DetailStore,
    context: &mut LegacyContext<'_>,
    selection: LegacyConjugationSelection,
    source_route: Route,
) -> Attempt<Vec<LegacyConjugation>> {
    if selection == LegacyConjugationSelection::Root {
        return Ok(Vec::new());
    }
    let inflected = members
        .iter()
        .filter(|member| !member.inflection.is_empty())
        .collect::<Vec<_>>();
    let selected = if selection == LegacyConjugationSelection::Default
        && inflected.iter().any(|member| member.inflection.len() == 1)
    {
        inflected
            .into_iter()
            .filter(|member| member.inflection.len() == 1)
            .collect()
    } else {
        inflected
    };
    let initial = selected
        .into_iter()
        .enumerate()
        .map(|(order, member)| StageItem {
            member,
            depth: member.inflection.len() - 1,
            order,
        })
        .collect::<Vec<_>>();
    if initial.is_empty() {
        return Ok(Vec::new());
    }
    render(&initial, session, details, context, source_route)
}

fn render(
    items: &[StageItem<'_>],
    session: &mut LegacyDetailedSession,
    details: &DetailStore,
    context: &mut LegacyContext<'_>,
    source_route: Route,
) -> Attempt<Vec<LegacyConjugation>> {
    let mut nodes = Vec::new();
    for row in merge_via_rows(stage_rows(items)) {
        let prop = row.properties.iter().map(conj_property).collect();
        let flags = row
            .properties
            .iter()
            .map(|value| LegacyConjugationFlags {
                negative: value.negative,
                formal: value.formal,
            })
            .collect();
        if row.via {
            let nested = row
                .items
                .iter()
                .map(|item| StageItem {
                    member: item.member,
                    depth: item.depth - 1,
                    order: item.order,
                })
                .collect::<Vec<_>>();
            let via = render(&nested, session, details, context, source_route)?;
            if !via.is_empty() {
                let readok = via.first().and_then(|node| node.readok);
                nodes.push(LegacyConjugation {
                    prop,
                    reading: None,
                    gloss: None,
                    via: Some(via),
                    readok,
                    info: Some(LegacyConjugationInfo {
                        flags,
                        short_gloss: None,
                    }),
                    root: None,
                });
            }
            continue;
        }
        let member = row.items[0].member;
        let Some(root) = member.root.as_ref() else {
            continue;
        };
        let presentation = legacy_source_root(root, source_route);
        let entry = session.entry(member.entry_index, details)?.cloned();
        let label_route = if test_word(
            &presentation.form.encode_utf16().collect::<Vec<_>>(),
            CharClass::Kana,
        ) {
            Route::Kana
        } else {
            Route::Kanji
        };
        let hinted = context.annotations.hint(
            presentation.seq,
            Route::Kana,
            &presentation.reading,
            &presentation.reading,
        )?;
        let hinted = match hinted {
            Some(hinted) => hinted,
            None => context
                .annotations
                .hint(
                    presentation.seq,
                    label_route,
                    &presentation.form,
                    &presentation.reading,
                )?
                .unwrap_or_else(|| presentation.reading.clone()),
        };
        let positions = row
            .properties
            .iter()
            .map(|property| property.pos.clone())
            .collect::<Vec<_>>();
        let gloss = match (entry.as_ref(), member.entry_index) {
            (Some(entry), Some(index)) => senses(
                entry,
                index,
                context,
                source_route,
                &presentation.form,
                &presentation.reading,
                Some(&positions),
            )?,
            _ => Vec::new(),
        };
        nodes.push(LegacyConjugation {
            prop,
            reading: Some(reading_label(label_route, &presentation.form, &hinted)),
            gloss: Some(gloss),
            via: None,
            readok: Some(true),
            info: Some(LegacyConjugationInfo {
                flags,
                short_gloss: Some(String::new()),
            }),
            root: Some(AnalysisRoot {
                seq: presentation.seq,
                form: presentation.form,
                reading: hinted,
            }),
        });
    }
    let readable = nodes
        .iter()
        .filter(|node| node.readok == Some(true))
        .cloned()
        .collect::<Vec<_>>();
    Ok(if readable.is_empty() { nodes } else { readable })
}

fn stage_rows<'a>(items: &[StageItem<'a>]) -> Vec<StageRow<'a>> {
    let roots_with_via = items
        .iter()
        .filter(|item| item.depth > 0)
        .map(root_identity)
        .collect::<HashSet<_>>();
    let active = items
        .iter()
        .filter(|item| item.depth > 0 || !roots_with_via.contains(&root_identity(item)))
        .cloned()
        .collect::<Vec<_>>();
    let mut grouped: Vec<(StageIdentity, Vec<StageItem<'a>>)> = Vec::new();
    for item in active {
        let group = item.member.stage_groups.get(item.depth).copied().flatten();
        let key = item
            .member
            .stage_keys
            .get(item.depth)
            .and_then(|value| value.as_ref());
        let member_ord = item
            .member
            .stage_member_ords
            .get(item.depth)
            .copied()
            .flatten();
        let identity = match (group, key, member_ord) {
            (Some(group), _, Some(member)) => StageIdentity::Group(group, member),
            (None, Some(key), Some(member)) => {
                StageIdentity::Semantic(key.clone(), item.depth, member)
            }
            _ => StageIdentity::Unique(item.order, item.depth),
        };
        if let Some((_, values)) = grouped.iter_mut().find(|(key, _)| *key == identity) {
            values.push(item);
        } else {
            grouped.push((identity, vec![item]));
        }
    }
    let mut rows = grouped
        .into_iter()
        .map(|(_, values)| {
            let mut ordered = values.clone();
            ordered.sort_by_key(|item| {
                (
                    item.member
                        .stage_prop_ords
                        .get(item.depth)
                        .copied()
                        .flatten()
                        .unwrap_or(u16::MAX),
                    item.order,
                )
            });
            let mut seen = Vec::new();
            let mut properties = Vec::new();
            for item in ordered {
                let property = item.member.inflection[item.depth].clone();
                let identity = item
                    .member
                    .stage_prop_ords
                    .get(item.depth)
                    .copied()
                    .flatten()
                    .map_or_else(
                        || PropertyIdentity::Value(property.clone()),
                        PropertyIdentity::Ord,
                    );
                if !seen.contains(&identity) {
                    seen.push(identity);
                    properties.push(property);
                }
            }
            StageRow {
                via: values[0].depth > 0,
                member_ord: values[0]
                    .member
                    .stage_member_ords
                    .get(values[0].depth)
                    .copied()
                    .flatten(),
                order: values.iter().map(|item| item.order).min().unwrap_or(0),
                items: values,
                properties,
            }
        })
        .collect::<Vec<_>>();
    rows.sort_by(compare_rows);
    rows
}

fn compare_rows(left: &StageRow<'_>, right: &StageRow<'_>) -> Ordering {
    left.via
        .cmp(&right.via)
        .then_with(|| {
            min_conjugation_order(&left.properties).cmp(&min_conjugation_order(&right.properties))
        })
        .then_with(|| {
            left.member_ord
                .unwrap_or(u8::MAX)
                .cmp(&right.member_ord.unwrap_or(u8::MAX))
        })
        .then_with(|| left.order.cmp(&right.order))
}

fn merge_via_rows(mut rows: Vec<StageRow<'_>>) -> Vec<StageRow<'_>> {
    let mut output: Vec<StageRow<'_>> = Vec::new();
    for row in rows.drain(..) {
        if !row.via {
            output.push(row);
            continue;
        }
        let first = &row.items[0];
        let via_group = first
            .member
            .stage_groups
            .get(first.depth - 1)
            .copied()
            .flatten();
        let Some(via_group) = via_group else {
            output.push(row);
            continue;
        };
        if let Some(prior) = output.iter_mut().find(|prior| {
            prior.via
                && prior.items[0]
                    .member
                    .stage_groups
                    .get(prior.items[0].depth - 1)
                    .copied()
                    .flatten()
                    == Some(via_group)
        }) {
            prior.items.extend(row.items);
        } else {
            output.push(row);
        }
    }
    output
}

fn root_identity(item: &StageItem<'_>) -> String {
    item.member.root.as_ref().map_or_else(
        || format!("unique:{}", item.order),
        |root| root.seq.to_string(),
    )
}

fn conj_property(value: &MorphologyProperty) -> LegacyConjugationProperty {
    LegacyConjugationProperty {
        pos: value.pos.clone(),
        kind: descriptions::conjugation(value.kind),
        fml: (value.formal == Some(true)).then_some(true),
        neg: (value.negative == Some(true)).then_some(true),
        kind_id: value.kind,
    }
}

fn min_conjugation_order(properties: &[MorphologyProperty]) -> u8 {
    properties
        .iter()
        .map(|property| match property.kind {
            10 => 13,
            13 => 10,
            value => value,
        })
        .min()
        .unwrap_or(u8::MAX)
}

fn legacy_source_root(root: &AnalysisRoot, source_route: Route) -> AnalysisRoot {
    if source_route != Route::Kana {
        return root.clone();
    }
    match root.seq {
        1_547_720 => AnalysisRoot {
            seq: root.seq,
            form: "来る".to_owned(),
            reading: "クる".to_owned(),
        },
        2_827_915 => AnalysisRoot {
            seq: root.seq,
            form: "置けばよい".to_owned(),
            reading: "おけばよい".to_owned(),
        },
        _ => root.clone(),
    }
}

fn reading_label(route: Route, text: &str, reading: &str) -> String {
    if route == Route::Kanji {
        format!("{text} 【{reading}】")
    } else {
        text.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn property(kind: u8) -> MorphologyProperty {
        MorphologyProperty {
            pos: "v1".to_owned(),
            kind,
            negative: Some(false),
            formal: Some(false),
            ordinal: 0,
        }
    }

    fn member(
        seq: u32,
        inflection: Vec<MorphologyProperty>,
        groups: Vec<Option<u32>>,
        members: Vec<Option<u8>>,
        properties: Vec<Option<u16>>,
    ) -> LegacySemanticMember {
        LegacySemanticMember {
            entry_index: None,
            root: Some(AnalysisRoot {
                seq,
                form: seq.to_string(),
                reading: seq.to_string(),
            }),
            inflection,
            stage_groups: groups,
            stage_keys: Vec::new(),
            stage_member_ords: members,
            stage_prop_ords: properties,
            member_ord: None,
        }
    }

    #[test]
    fn physical_rows_keep_property_order_and_suppress_only_same_root_direct_rows() {
        let same_row_a = member(
            100,
            vec![property(8)],
            vec![Some(10)],
            vec![Some(1)],
            vec![Some(1)],
        );
        let same_row_b = member(
            100,
            vec![property(6)],
            vec![Some(10)],
            vec![Some(1)],
            vec![Some(0)],
        );
        let rows = stage_rows(&[
            StageItem {
                member: &same_row_a,
                depth: 0,
                order: 0,
            },
            StageItem {
                member: &same_row_b,
                depth: 0,
                order: 1,
            },
        ]);
        assert_eq!(rows.len(), 1);
        assert_eq!(
            rows[0]
                .properties
                .iter()
                .map(|value| value.kind)
                .collect::<Vec<_>>(),
            [6, 8]
        );

        let direct = member(
            100,
            vec![property(2)],
            vec![Some(20)],
            vec![Some(1)],
            vec![Some(0)],
        );
        let via = member(
            100,
            vec![property(5), property(2)],
            vec![Some(10), Some(20)],
            vec![Some(2), Some(3)],
            vec![Some(0), Some(0)],
        );
        let unrelated = member(
            300,
            vec![property(2)],
            vec![Some(30)],
            vec![Some(1)],
            vec![Some(0)],
        );
        let rows = stage_rows(&[
            StageItem {
                member: &direct,
                depth: 0,
                order: 0,
            },
            StageItem {
                member: &via,
                depth: 1,
                order: 1,
            },
            StageItem {
                member: &unrelated,
                depth: 0,
                order: 2,
            },
        ]);
        assert_eq!(rows.len(), 2);
        assert!(!rows[0].via);
        assert_eq!(rows[0].items[0].member.root.as_ref().unwrap().seq, 300);
        assert!(rows[1].via);
    }

    #[test]
    fn repeated_outer_rows_merge_only_when_the_via_group_matches() {
        let first = member(
            100,
            vec![property(5), property(2)],
            vec![Some(10), Some(20)],
            vec![Some(1), Some(3)],
            vec![Some(0), Some(0)],
        );
        let second = member(
            300,
            vec![property(6), property(2)],
            vec![Some(10), Some(21)],
            vec![Some(2), Some(4)],
            vec![Some(0), Some(0)],
        );
        let rows = merge_via_rows(stage_rows(&[
            StageItem {
                member: &first,
                depth: 1,
                order: 0,
            },
            StageItem {
                member: &second,
                depth: 1,
                order: 1,
            },
        ]));
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].items.len(), 2);
    }
}
