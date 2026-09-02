use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use serde_json::{Value, json};

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn native_batch_opens_once_and_emits_one_public_result_per_request() {
    let directory = PathBuf::from(
        std::env::var("ICHIRAN_M1_PACK_DIR")
            .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory"),
    );
    let input = json!([
        { "text": "猫", "limit": 1, "normalizePunctuation": false },
        {
            "text": "東京",
            "limit": 2,
            "entities": [{ "start": 0, "end": 2, "boost": 40 }],
            "normalizePunctuation": true
        }
    ]);
    let mut child = Command::new(env!("CARGO_BIN_EXE_analyzer_batch"))
        .arg(directory.join("hot.bin"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("start native analyzer batch");
    child
        .stdin
        .take()
        .expect("batch stdin")
        .write_all(input.to_string().as_bytes())
        .expect("write batch requests");
    let output = child.wait_with_output().expect("wait for native batch");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let results: Vec<Value> = serde_json::from_slice(&output.stdout).expect("parse batch output");
    assert_eq!(results.len(), 2);
    assert_eq!(results[0]["input"], "猫");
    assert_eq!(results[0]["computeMs"], 0);
    assert_eq!(results[1]["input"], "東京");
    assert_eq!(results[1]["paths"][0]["tokens"][0]["entity"], true);
}
