use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};

use serde_json::{Value, json};

#[test]
#[ignore = "requires an installed multilingual source-compiler release"]
fn detailed_batch_reads_the_bound_english_locale() {
    let directory = PathBuf::from(
        std::env::var("ICHIRAN_M1_PACK_DIR")
            .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory"),
    );
    let manifest: Value = serde_json::from_slice(
        &std::fs::read(directory.join("manifest.json")).expect("read release manifest"),
    )
    .expect("parse release manifest");
    let lexicon_sha256 = manifest["lexicon"]["installedSha256"]
        .as_str()
        .expect("lexicon installed digest");
    let request = json!([{
        "text": "猫",
        "limit": 1,
        "normalizePunctuation": true
    }]);
    let mut child = Command::new(env!("CARGO_BIN_EXE_analyzer_detailed_batch"))
        .arg(directory.join("hot.bin"))
        .arg(directory.join("lexicon.bin"))
        .arg(directory.join("gloss.en.bin"))
        .arg("en")
        .arg(lexicon_sha256)
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
    assert_eq!(results.len(), 1);
    assert!(results[0].is_array());
}
