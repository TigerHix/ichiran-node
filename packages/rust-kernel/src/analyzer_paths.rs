use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::sync::Arc;

use crate::analyzer_model::{
    EntityHint, PathPart, PathResult, PathTransition, RuleWordKind, Segment, SegmentGroup,
    SegmentRuleFacts,
};
use crate::analyzer_rules::{resolve_initial_rules, resolve_rule_transitions, short_penalty_left};

pub const GAP_PENALTY: f64 = -500.0;
pub const DEFAULT_ENTITY_BOOST: f64 = 50.0;
const MAX_LIMIT: usize = 10;
const MAX_ENTITIES: usize = 64;
const MAX_ENTITY_ABS_BOOST: f64 = 1_000_000.0;

type InitialResolver<'a> = dyn Fn(&SegmentGroup) -> Vec<SegmentGroup> + 'a;
type TransitionResolver<'a> = dyn Fn(&SegmentGroup, &SegmentGroup) -> Vec<PathTransition> + 'a;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct EventOrder {
    source_index: isize,
    destination_index: isize,
    prior_rank: usize,
    split_rank: usize,
}

#[derive(Clone, Debug)]
struct PathNode {
    part: PathPart,
    next: Option<Arc<Self>>,
}

#[derive(Clone, Debug)]
struct TopItem {
    score: f64,
    path: Option<Arc<PathNode>>,
    order: Option<EventOrder>,
}

struct CompletedItem {
    score: f64,
    path: Option<Arc<PathNode>>,
    order: EventOrder,
}

#[derive(Debug)]
struct TopItems {
    limit: usize,
    items: Vec<TopItem>,
}

impl TopItems {
    fn new(limit: usize) -> Self {
        Self {
            limit,
            items: Vec::with_capacity(limit),
        }
    }

    fn register(&mut self, item: TopItem) {
        let index = self
            .items
            .partition_point(|existing| existing.score >= item.score);
        if index < self.limit {
            self.items.insert(index, item);
            self.items.truncate(self.limit);
        }
    }

    fn values(&self) -> &[TopItem] {
        &self.items
    }
}

#[derive(Clone, Debug)]
struct PriorCandidate {
    source_index: usize,
    prior_rank: usize,
    prior: TopItem,
    head: SegmentGroup,
    left_score: f64,
    short_left: bool,
}

#[derive(Debug)]
struct MetricItem {
    key: f64,
    candidate: PriorCandidate,
}

#[derive(Debug)]
struct MetricFrontier {
    limit: usize,
    items: Vec<MetricItem>,
}

impl MetricFrontier {
    fn new(limit: usize) -> Self {
        Self {
            limit,
            items: Vec::with_capacity(limit + 1),
        }
    }

    fn register(&mut self, candidate: PriorCandidate, key: f64) {
        self.items.push(MetricItem { key, candidate });
        self.items.sort_by(|left, right| {
            right
                .key
                .total_cmp(&left.key)
                .then_with(|| {
                    left.candidate
                        .source_index
                        .cmp(&right.candidate.source_index)
                })
                .then_with(|| left.candidate.prior_rank.cmp(&right.candidate.prior_rank))
        });
        self.items.truncate(self.limit);
    }
}

pub fn gap_penalty(start: usize, end: usize) -> f64 {
    (end - start) as f64 * GAP_PENALTY
}

fn group_score(group: &SegmentGroup) -> f64 {
    group.segments.first().map_or(0.0, |segment| segment.score)
}

fn entity_boost(group: &SegmentGroup, entities: &[EntityHint]) -> f64 {
    entities
        .iter()
        .filter(|entity| entity.start == group.start && entity.end == group.end)
        .map(|entity| entity.boost.unwrap_or(DEFAULT_ENTITY_BOOST))
        .sum()
}

fn prepared_groups(groups: &[SegmentGroup]) -> Vec<SegmentGroup> {
    let mut groups = groups.to_vec();
    for group in &mut groups {
        group
            .segments
            .sort_by(|left, right| right.score.total_cmp(&left.score));
    }
    groups.sort_by(|left, right| {
        left.start
            .cmp(&right.start)
            .then_with(|| left.end.cmp(&right.end))
    });
    groups
}

pub fn add_entity_groups(
    groups: &[SegmentGroup],
    entities: &[EntityHint],
    input_text: &[u16],
) -> Vec<SegmentGroup> {
    let mut group_id = groups
        .iter()
        .fold(0_i64, |minimum, group| minimum.min(group.group_id))
        - 1;
    let mut candidate_id = -1_i64;
    let mut result = groups.to_vec();
    for entity in entities {
        let boost = entity.boost.unwrap_or(DEFAULT_ENTITY_BOOST);
        result.push(SegmentGroup {
            group_id,
            start: entity.start,
            end: entity.end,
            segments: vec![Segment {
                candidate_id,
                start: entity.start,
                end: entity.end,
                score: boost,
                common: None,
                entity: true,
                rules: Some(SegmentRuleFacts {
                    text: input_text
                        .get(entity.start..entity.end)
                        .unwrap_or_default()
                        .to_vec(),
                    word_kind: RuleWordKind::Simple,
                    score_info: None,
                    compound_end_seq: None,
                    compound_end_text: None,
                }),
            }],
            matches: 1,
        });
        group_id -= 1;
        candidate_id -= 1;
    }
    prepared_groups(&result)
}

fn transition_path(prior: &TopItem, split: &PathTransition) -> Arc<PathNode> {
    let replaced_left = Arc::new(PathNode {
        part: PathPart::Group(split.left.clone()),
        next: prior.path.as_ref().and_then(|path| path.next.clone()),
    });
    let adjustment = if let Some(adjustment) = &split.adjustment {
        Arc::new(PathNode {
            part: PathPart::Adjustment(adjustment.clone()),
            next: Some(replaced_left),
        })
    } else {
        replaced_left
    };
    Arc::new(PathNode {
        part: PathPart::Group(split.right.clone()),
        next: Some(adjustment),
    })
}

fn materialize_path(mut path: Option<Arc<PathNode>>) -> Vec<PathPart> {
    let mut newest_first = Vec::new();
    while let Some(node) = path.take() {
        newest_first.push(node.part.clone());
        path = node.next.clone();
    }
    newest_first.reverse();
    newest_first
}

fn compare_event_order(left: EventOrder, right: EventOrder) -> Ordering {
    left.source_index
        .cmp(&right.source_index)
        .then_with(|| left.destination_index.cmp(&right.destination_index))
        .then_with(|| left.prior_rank.cmp(&right.prior_rank))
        .then_with(|| left.split_rank.cmp(&right.split_rank))
}

fn find_default_paths(
    groups: &[SegmentGroup],
    text_length: usize,
    limit: usize,
    entities: &[EntityHint],
    initial: &InitialResolver<'_>,
) -> Result<Vec<PathResult>, String> {
    let mut group_tops: Vec<_> = groups.iter().map(|_| TopItems::new(limit)).collect();
    let mut candidates_by_group: Vec<Vec<PriorCandidate>> =
        groups.iter().map(|_| Vec::new()).collect();
    let mut indexes_by_end: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    for (index, group) in groups.iter().enumerate() {
        indexes_by_end.entry(group.end).or_default().push(index);
    }
    let mut activation_order: Vec<_> = (0..groups.len()).collect();
    activation_order.sort_by_key(|index| (groups[*index].end, *index));
    let mut activation_index = 0;
    let mut normal_score = MetricFrontier::new(limit);
    let mut short_score = MetricFrontier::new(limit);
    let mut score_without_left = MetricFrontier::new(limit);

    for (right_index, second) in groups.iter().enumerate() {
        while activation_index < activation_order.len()
            && groups[activation_order[activation_index]].end < second.start
        {
            let source_index = activation_order[activation_index];
            activation_index += 1;
            for candidate in &candidates_by_group[source_index] {
                let gap_credit = -GAP_PENALTY * candidate.head.end as f64;
                let frontier = if candidate.short_left {
                    &mut short_score
                } else {
                    &mut normal_score
                };
                frontier.register(candidate.clone(), candidate.prior.score + gap_credit);
                score_without_left.register(
                    candidate.clone(),
                    candidate.prior.score + gap_credit - candidate.left_score,
                );
            }
        }

        let mut incoming = BTreeMap::new();
        for item in normal_score
            .items
            .iter()
            .chain(&short_score.items)
            .chain(&score_without_left.items)
        {
            incoming
                .entry((item.candidate.source_index, item.candidate.prior_rank))
                .or_insert_with(|| item.candidate.clone());
        }
        if let Some(source_indexes) = indexes_by_end.get(&second.start) {
            for &source_index in source_indexes {
                if source_index >= right_index {
                    break;
                }
                for candidate in &candidates_by_group[source_index] {
                    incoming
                        .entry((candidate.source_index, candidate.prior_rank))
                        .or_insert_with(|| candidate.clone());
                }
            }
        }

        let second_score = group_score(second);
        let second_entity_boost = entity_boost(second, entities);
        let second_top = &mut group_tops[right_index];
        for candidate in incoming.values() {
            let resolved = resolve_rule_transitions(&candidate.head, second);
            let pair_gap = gap_penalty(candidate.head.end, second.start);
            let score_tail = candidate.prior.score - candidate.left_score;
            for (split_rank, split) in resolved.iter().enumerate() {
                let split_score = group_score(&split.right)
                    + split.adjustment.as_ref().map_or(0.0, |value| value.score)
                    + group_score(&split.left)
                    + second_entity_boost;
                let accumulated = pair_gap
                    + split_score
                        .max(candidate.left_score + 1.0)
                        .max(second_score + 1.0)
                    + score_tail;
                second_top.register(TopItem {
                    score: accumulated,
                    path: Some(transition_path(&candidate.prior, split)),
                    order: Some(EventOrder {
                        source_index: candidate.source_index as isize,
                        destination_index: right_index as isize,
                        prior_rank: candidate.prior_rank,
                        split_rank,
                    }),
                });
            }
        }

        let gap_left = gap_penalty(0, second.start);
        for (initial_rank, filtered) in initial(second).into_iter().enumerate() {
            let score = group_score(&filtered) + second_entity_boost;
            second_top.register(TopItem {
                score: gap_left + score,
                path: Some(Arc::new(PathNode {
                    part: PathPart::Group(filtered),
                    next: None,
                })),
                order: Some(EventOrder {
                    source_index: right_index as isize,
                    destination_index: -1,
                    prior_rank: initial_rank,
                    split_rank: 0,
                }),
            });
        }

        let mut candidates = Vec::with_capacity(second_top.values().len());
        for (prior_rank, prior) in second_top.values().iter().enumerate() {
            let Some(PathPart::Group(head)) = prior.path.as_ref().map(|path| &path.part) else {
                return Err("analyzer path has no final segment group".to_owned());
            };
            candidates.push(PriorCandidate {
                source_index: right_index,
                prior_rank,
                prior: prior.clone(),
                head: head.clone(),
                left_score: group_score(head),
                short_left: short_penalty_left(head),
            });
        }
        candidates_by_group[right_index] = candidates;
    }

    let mut completed = vec![CompletedItem {
        score: gap_penalty(0, text_length),
        path: None,
        order: EventOrder {
            source_index: -1,
            destination_index: -1,
            prior_rank: 0,
            split_rank: 0,
        },
    }];
    for (index, group) in groups.iter().enumerate() {
        let final_gap = gap_penalty(group.end, text_length);
        for item in group_tops[index].values() {
            let Some(order) = item.order else {
                return Err("analyzer path has no event order".to_owned());
            };
            completed.push(CompletedItem {
                score: item.score + final_gap,
                path: item.path.clone(),
                order,
            });
        }
    }
    completed.sort_by(|left, right| {
        right
            .score
            .total_cmp(&left.score)
            .then_with(|| compare_event_order(left.order, right.order))
    });
    Ok(completed
        .into_iter()
        .take(limit)
        .map(|item| PathResult {
            score: item.score,
            parts: materialize_path(item.path),
        })
        .collect())
}

fn find_exhaustive_paths(
    groups: &[SegmentGroup],
    text_length: usize,
    limit: usize,
    entities: &[EntityHint],
    initial: &InitialResolver<'_>,
    transition: &TransitionResolver<'_>,
) -> Vec<PathResult> {
    let mut top = TopItems::new(limit);
    let mut group_tops: Vec<_> = groups.iter().map(|_| TopItems::new(limit)).collect();
    top.register(TopItem {
        score: gap_penalty(0, text_length),
        path: None,
        order: None,
    });

    for (left_index, first) in groups.iter().enumerate() {
        let (_, at_and_after) = group_tops.split_at_mut(left_index);
        let Some((first_top, after)) = at_and_after.split_first_mut() else {
            continue;
        };
        let gap_left = gap_penalty(0, first.start);
        let gap_right = gap_penalty(first.end, text_length);
        let first_entity_boost = entity_boost(first, entities);
        for filtered in initial(first) {
            let score = group_score(&filtered) + first_entity_boost;
            let path = Some(Arc::new(PathNode {
                part: PathPart::Group(filtered),
                next: None,
            }));
            let item = TopItem {
                score: gap_left + score,
                path: path.clone(),
                order: None,
            };
            first_top.register(item);
            top.register(TopItem {
                score: gap_left + score + gap_right,
                path,
                order: None,
            });
        }

        let prior_values = first_top.values().to_vec();
        for (offset, second) in groups[left_index + 1..].iter().enumerate() {
            if second.start < first.end {
                continue;
            }
            let second_top = &mut after[offset];
            let second_score = group_score(second);
            let pair_gap = gap_penalty(first.end, second.start);
            let final_gap = gap_penalty(second.end, text_length);
            let second_entity_boost = entity_boost(second, entities);
            for prior in &prior_values {
                let Some(PathPart::Group(prior_head)) = prior.path.as_ref().map(|path| &path.part)
                else {
                    continue;
                };
                let left_score = group_score(prior_head);
                let score_tail = prior.score - left_score;
                for split in transition(prior_head, second) {
                    let split_score = group_score(&split.right)
                        + split.adjustment.as_ref().map_or(0.0, |value| value.score)
                        + group_score(&split.left)
                        + second_entity_boost;
                    let accumulated = pair_gap
                        + split_score.max(left_score + 1.0).max(second_score + 1.0)
                        + score_tail;
                    let path = Some(transition_path(prior, &split));
                    second_top.register(TopItem {
                        score: accumulated,
                        path: path.clone(),
                        order: None,
                    });
                    top.register(TopItem {
                        score: accumulated + final_gap,
                        path,
                        order: None,
                    });
                }
            }
        }
    }

    top.values()
        .iter()
        .map(|item| PathResult {
            score: item.score,
            parts: materialize_path(item.path.clone()),
        })
        .collect()
}

fn validate_options(
    groups: &[SegmentGroup],
    text_length: usize,
    limit: usize,
    entities: &[EntityHint],
) -> Result<(), String> {
    if !(1..=MAX_LIMIT).contains(&limit) {
        return Err(format!("limit must be an integer from 1 to {MAX_LIMIT}"));
    }
    if entities.len() > MAX_ENTITIES {
        return Err(format!(
            "entities must contain at most {MAX_ENTITIES} hints"
        ));
    }
    if groups
        .iter()
        .flat_map(|group| &group.segments)
        .any(|segment| !segment.score.is_finite())
    {
        return Err("segment scores must be finite".to_owned());
    }
    for (index, entity) in entities.iter().enumerate() {
        if entity.end <= entity.start || entity.end > text_length {
            return Err(format!(
                "entities[{index}] must be a non-empty span within the input"
            ));
        }
        if entity.boost.is_some_and(|boost| {
            !boost.is_finite() || !(-MAX_ENTITY_ABS_BOOST..=MAX_ENTITY_ABS_BOOST).contains(&boost)
        }) {
            return Err(format!(
                "entities[{index}].boost must be finite and between -{MAX_ENTITY_ABS_BOOST} and {MAX_ENTITY_ABS_BOOST}"
            ));
        }
    }
    Ok(())
}

pub fn find_paths(
    input_groups: &[SegmentGroup],
    text_length: usize,
    limit: usize,
    entities: &[EntityHint],
) -> Result<Vec<PathResult>, String> {
    find_paths_with(input_groups, text_length, limit, entities, None, None)
}

pub fn find_paths_with(
    input_groups: &[SegmentGroup],
    text_length: usize,
    limit: usize,
    entities: &[EntityHint],
    initial: Option<&InitialResolver<'_>>,
    transition: Option<&TransitionResolver<'_>>,
) -> Result<Vec<PathResult>, String> {
    validate_options(input_groups, text_length, limit, entities)?;
    let groups = prepared_groups(input_groups);
    let custom_initial = initial.is_some();
    let default_initial = |group: &SegmentGroup| resolve_initial_rules(group);
    let initial = initial.unwrap_or(&default_initial);
    if transition.is_none() && !custom_initial {
        return find_default_paths(&groups, text_length, limit, entities, initial);
    }
    if transition.is_none() {
        let default_transition =
            |left: &SegmentGroup, right: &SegmentGroup| resolve_rule_transitions(left, right);
        return Ok(find_exhaustive_paths(
            &groups,
            text_length,
            limit,
            entities,
            initial,
            &default_transition,
        ));
    }
    let Some(transition) = transition else {
        return Err("analyzer transition resolver is missing".to_owned());
    };
    Ok(find_exhaustive_paths(
        &groups,
        text_length,
        limit,
        entities,
        initial,
        transition,
    ))
}

#[cfg(test)]
mod tests;
