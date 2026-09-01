use std::fs;
use std::path::PathBuf;

use ichiran_kernel::{AnalyzeOptions, EntityHint, Kernel};
use serde_json::{Map, Value, json};

fn object(value: &Value) -> &Map<String, Value> {
    value.as_object().expect("fixture value must be an object")
}

fn text(value: &Value, key: &str) -> String {
    object(value)[key]
        .as_str()
        .expect("fixture text must be a string")
        .to_owned()
}

fn field(value: &Value, key: &str) -> Value {
    object(value).get(key).cloned().unwrap_or(Value::Null)
}

fn clean_component(value: &Value) -> Value {
    json!({
        "text": field(value, "text"),
        "trueText": field(value, "trueText"),
        "route": field(value, "route"),
        "reading": field(value, "reading"),
        "readings": [field(value, "reading")],
        "root": field(value, "root"),
        "inflection": field(value, "inflection"),
        "primary": field(value, "primary")
    })
}

fn clean_alternative(value: &Value) -> Value {
    json!({
        "text": field(value, "text"),
        "trueText": field(value, "trueText"),
        "route": field(value, "route"),
        "reading": field(value, "reading"),
        "readings": [field(value, "reading")],
        "score": field(value, "score"),
        "root": field(value, "root"),
        "inflection": field(value, "inflection"),
        "components": field(value, "components")
            .as_array()
            .expect("components must be an array")
            .iter()
            .map(clean_component)
            .collect::<Vec<_>>(),
        "counter": field(value, "counter")
    })
}

fn clean_token(value: &Value) -> Value {
    let alternatives = field(value, "alternatives")
        .as_array()
        .expect("alternatives must be an array")
        .iter()
        .map(clean_alternative)
        .collect::<Vec<_>>();
    let mut readings = Vec::new();
    for alternative in &alternatives {
        let reading = field(alternative, "reading");
        if !readings.contains(&reading) {
            readings.push(reading);
        }
    }
    if readings.is_empty() {
        readings.push(field(value, "reading"));
    }
    json!({
        "start": field(value, "start"),
        "end": field(value, "end"),
        "text": field(value, "text"),
        "trueText": field(value, "trueText"),
        "route": field(value, "route"),
        "reading": field(value, "reading"),
        "readings": readings,
        "score": field(value, "score"),
        "root": field(value, "root"),
        "inflection": field(value, "inflection"),
        "components": field(value, "components")
            .as_array()
            .expect("components must be an array")
            .iter()
            .map(clean_component)
            .collect::<Vec<_>>(),
        "alternatives": alternatives,
        "skipped": field(value, "skipped"),
        "entity": field(value, "entity"),
        "counter": field(value, "counter")
    })
}

fn clean_path(value: &Value) -> Value {
    json!({
        "score": field(value, "score"),
        "tokens": field(value, "tokens")
            .as_array()
            .expect("tokens must be an array")
            .iter()
            .map(clean_token)
            .collect::<Vec<_>>()
    })
}

fn clean_result(value: Value) -> Value {
    json!({
        "input": field(&value, "input"),
        "normalized": field(&value, "normalized"),
        "chunks": field(&value, "chunks")
            .as_array()
            .expect("chunks must be an array")
            .iter()
            .map(|chunk| if field(chunk, "type") == "misc" {
                json!({
                    "type": "misc",
                    "start": field(chunk, "start"),
                    "end": field(chunk, "end"),
                    "text": field(chunk, "text")
                })
            } else {
                json!({
                    "type": "word",
                    "start": field(chunk, "start"),
                    "end": field(chunk, "end"),
                    "text": field(chunk, "text"),
                    "paths": field(chunk, "paths")
                        .as_array()
                        .expect("chunk paths must be an array")
                        .iter()
                        .map(clean_path)
                        .collect::<Vec<_>>()
                })
            })
            .collect::<Vec<_>>(),
        "paths": field(&value, "paths")
            .as_array()
            .expect("paths must be an array")
            .iter()
            .map(clean_path)
            .collect::<Vec<_>>()
    })
}

fn semantic_candidate_key(value: &Value) -> String {
    let root = field(value, "root");
    let root = root.as_object();
    serde_json::to_string(&json!([
        field(value, "route"),
        field(value, "text"),
        root.and_then(|value| value.get("seq"))
            .cloned()
            .unwrap_or(Value::Null),
        root.and_then(|value| value.get("form"))
            .cloned()
            .unwrap_or(Value::Null),
        root.and_then(|value| value.get("reading"))
            .cloned()
            .unwrap_or(Value::Null),
        field(value, "inflection"),
        field(value, "components")
            .as_array()
            .map_or_else(Vec::new, |values| values
                .iter()
                .map(semantic_candidate_key)
                .collect())
    ]))
    .expect("serialize semantic candidate key")
}

fn score(value: &Value) -> Option<f64> {
    value
        .as_object()
        .and_then(|value| value.get("score"))
        .and_then(Value::as_f64)
}

fn sort_equal_score_runs(values: &mut [Value], key: impl Fn(&Value) -> String) {
    let mut start = 0;
    while start < values.len() {
        let Some(current) = score(&values[start]) else {
            start += 1;
            continue;
        };
        let mut end = start + 1;
        while end < values.len() && score(&values[end]) == Some(current) {
            end += 1;
        }
        values[start..end].sort_by_key(&key);
        start = end;
    }
}

fn path_semantic_key(value: &Value) -> String {
    serde_json::to_string(
        &field(value, "tokens")
            .as_array()
            .map_or_else(Vec::new, |tokens| {
                tokens.iter().map(semantic_candidate_key).collect()
            }),
    )
    .expect("serialize semantic path key")
}

fn canonicalize(value: &mut Value) {
    match value {
        Value::Array(values) => {
            for child in values.iter_mut() {
                canonicalize(child);
            }
            if !values.is_empty()
                && values
                    .iter()
                    .all(|child| score(child).is_some() && object(child).contains_key("tokens"))
            {
                sort_equal_score_runs(values, path_semantic_key);
            }
        }
        Value::Object(values) => {
            for child in values.values_mut() {
                canonicalize(child);
            }
            if let Some(Value::Array(alternatives)) = values.get_mut("alternatives") {
                sort_equal_score_runs(alternatives, semantic_candidate_key);
            }
        }
        _ => {}
    }
}

fn entity(value: &Value) -> EntityHint {
    EntityHint {
        start: field(value, "start").as_u64().expect("entity start") as usize,
        end: field(value, "end").as_u64().expect("entity end") as usize,
        boost: object(value).get("boost").and_then(Value::as_f64),
    }
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn all_301_provenance_bound_fallback_clean_results_are_exact() {
    let directory = PathBuf::from(
        std::env::var("ICHIRAN_M1_PACK_DIR")
            .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory"),
    );
    let mut kernel = Kernel::open(fs::read(directory.join("hot.bin")).expect("read hot.bin"))
        .expect("open qualified hot pack");
    let fixture: Value = serde_json::from_str(include_str!("fixtures/m3-fallback.json"))
        .expect("parse fallback fixture");
    let suites = object(&fixture["suites"]);
    let mut compared = 0;
    for suite in ["counters", "entities", "probes"] {
        for (index, case) in suites[suite]
            .as_array()
            .expect("fixture suite must be an array")
            .iter()
            .enumerate()
        {
            let request = &case["request"];
            let input = text(request, "text");
            let options = AnalyzeOptions {
                limit: field(request, "limit").as_u64().expect("request limit") as usize,
                entities: object(case)
                    .get("entities")
                    .and_then(Value::as_array)
                    .map_or_else(Vec::new, |values| values.iter().map(entity).collect()),
                normalize_punctuation: object(request)
                    .get("normalizePunctuation")
                    .and_then(Value::as_bool)
                    .unwrap_or(true),
            };
            let actual = kernel
                .analyze_with_options(&input.encode_utf16().collect::<Vec<_>>(), &options)
                .unwrap_or_else(|error| panic!("{suite}[{index}] {input:?}: {error}"));
            let mut actual = clean_result(serde_json::to_value(actual).expect("serialize result"));
            let mut expected = case["clean"].clone();
            canonicalize(&mut actual);
            canonicalize(&mut expected);
            assert_eq!(actual, expected, "{suite}[{index}] {input:?}");
            compared += 1;
        }
    }
    assert_eq!(compared, 301);
}
