use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use serde_json::{Value, json};

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn detailed_batch_matches_the_provenance_bound_counter_witness() {
    let directory = PathBuf::from(
        std::env::var("ICHIRAN_M1_PACK_DIR")
            .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory"),
    );
    let fixture: Value = serde_json::from_str(include_str!("fixtures/m3-fallback.json"))
        .expect("parse fallback fixture");
    let witness = &fixture["suites"]["counters"][0];
    let request = json!([{
        "text": witness["request"]["text"],
        "limit": witness["request"]["limit"],
        "normalizePunctuation": true
    }]);
    let mut child = Command::new(env!("CARGO_BIN_EXE_analyzer_detailed_batch"))
        .arg(directory.join("hot.bin"))
        .arg(directory.join("details.bin"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("start native detailed analyzer batch");
    child
        .stdin
        .take()
        .expect("batch stdin")
        .write_all(request.to_string().as_bytes())
        .expect("write batch request");
    let output = child.wait_with_output().expect("wait for detailed batch");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let results: Vec<Value> = serde_json::from_slice(&output.stdout).expect("parse batch output");
    assert_eq!(results, vec![witness["detailed"].clone()]);
}
