use sha2::{Digest, Sha256};

const FIXTURE: &str = include_str!("fixtures/m3-fallback.json");

#[test]
fn fallback_fixture_is_complete_and_provenance_bound() {
    let value: serde_json::Value = serde_json::from_str(FIXTURE).expect("valid fallback fixture");

    assert_eq!(value["formatVersion"], 1);
    assert_eq!(value["identityPolicy"], "terminal-root-v1");
    assert_eq!(
        value["source"]["sourcesLockSha256"],
        "80dc7c907d688a5ecb0bbd8b23b889f47cb3a28f8484f80e8dc4737bb090c070"
    );
    assert_eq!(value["source"]["dataReleaseTag"], "ichiran-260118");

    let suites = &value["suites"];
    assert_eq!(suites["counters"].as_array().unwrap().len(), 200);
    assert_eq!(suites["entities"].as_array().unwrap().len(), 54);
    assert_eq!(suites["probes"].as_array().unwrap().len(), 47);
    assert_eq!(value["counts"]["total"], 301);

    let digest = format!("{:x}", Sha256::digest(FIXTURE.as_bytes()));
    assert_eq!(
        digest,
        "dbc13ead615b8d70d2f3ecf38aeb7042361459856700a86844c5fe0db6706843"
    );
}
