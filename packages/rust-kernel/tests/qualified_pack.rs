use std::fs::{self, File};
use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};

use ichiran_kernel::{DetailStore, ErrorCode, Kernel, Pack, Route};
use serde::Deserialize;
use serde_json::Value;
use sha2::{Digest, Sha256};

fn release() -> PathBuf {
    std::env::var_os("ICHIRAN_M1_PACK_DIR")
        .map(PathBuf::from)
        .expect("ICHIRAN_M1_PACK_DIR must name the qualified release directory")
}

fn hot(directory: &Path) -> Vec<u8> {
    fs::read(directory.join("hot.bin")).expect("read qualified hot.bin")
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct M1OracleWitness {
    name: String,
    serialized: String,
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn strict_pack_and_real_section_counts() {
    let bytes = hot(&release());
    assert_eq!(bytes.len(), 24_857_288);
    assert_eq!(
        format!("{:x}", Sha256::digest(&bytes)),
        "61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0"
    );
    let pack = Pack::open(bytes).expect("strict outer pack");
    pack.verify_all().expect("all section checksums");
    assert_eq!(pack.manifest().format_version, 1);
    assert_eq!(pack.manifest().sections.len(), 5);
    assert_eq!(
        pack.manifest()
            .sections
            .iter()
            .map(|section| (
                section.id,
                section.offset,
                section.byte_length,
                section.checksum
            ))
            .collect::<Vec<_>>(),
        vec![
            (1, 152, 8_600_452, 0x1a4a91ed),
            (2, 8_600_608, 9_088_056, 0x576528bd),
            (3, 17_688_664, 2_688_176, 0x703014b6),
            (4, 20_376_840, 949_424, 0x22a66080),
            (5, 21_326_264, 3_531_024, 0x30844b91),
        ]
    );
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn strict_corruption_errors_are_typed() {
    let original = hot(&release());
    let mut bad_magic = original.clone();
    bad_magic[0] ^= 1;
    let Err(error) = Pack::open(bad_magic) else {
        panic!("bad magic was accepted")
    };
    assert_eq!(error.code, ErrorCode::InvalidHeader);

    let mut bad_directory = original.clone();
    bad_directory[32] ^= 1;
    let Err(error) = Pack::open(bad_directory) else {
        panic!("bad directory was accepted")
    };
    assert_eq!(error.code, ErrorCode::InvalidDirectory);

    let mut bad_section = original;
    bad_section[152] ^= 1;
    let Err(error) = Kernel::open(bad_section) else {
        panic!("bad section was accepted")
    };
    assert_eq!(error.code, ErrorCode::CorruptSection);
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn direct_and_morphology_outputs_match_the_typescript_oracle_exactly() {
    let mut kernel = Kernel::open(hot(&release())).expect("open Rust kernel");
    let oracle: Vec<M1OracleWitness> =
        serde_json::from_str(include_str!("fixtures/m1-oracle.json")).unwrap();
    for (name, text) in [("direct", "猫"), ("morphology", "食べた")] {
        let actual = serde_json::to_value(kernel.analyze_str(text, 1).unwrap()).unwrap();
        let expected = oracle
            .iter()
            .find(|witness| witness.name == name)
            .expect("missing native M1 oracle witness");
        let expected: Value = serde_json::from_str(&expected.serialized).unwrap();
        assert_eq!(actual, expected, "differential mismatch for {text}");
    }
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn generated_block_and_utf16_paths_are_real() {
    let mut kernel = Kernel::open(hot(&release())).expect("open Rust kernel");
    assert_eq!(kernel.generated_block_count(), 37);
    assert_eq!(kernel.generated_decoded_block_count(), 0);
    assert_eq!(kernel.generated_decoded_bytes(), 0);
    let public = serde_json::to_value(kernel.analyze_str("忘れた", 1).unwrap()).unwrap();
    assert!((1..kernel.generated_block_count()).contains(&kernel.generated_decoded_block_count()));
    assert!(kernel.generated_decoded_bytes() > 0);
    assert!(kernel.generated_decoded_bytes() < 9_417_412);
    kernel.preload_all_generated().unwrap();
    assert_eq!(kernel.generated_decoded_block_count(), 37);
    assert_eq!(kernel.generated_decoded_bytes(), 9_417_412);
    let generated = kernel
        .generated_lookup(&"忘れた".encode_utf16().collect::<Vec<_>>(), Route::Kanji)
        .unwrap();
    assert_eq!(generated.len(), 2);
    assert_eq!(generated[0].aliases, [581, 85]);
    assert_eq!(
        generated[0].facts.as_ref().unwrap().physical_group,
        Some(52_633)
    );
    assert_eq!(generated[1].aliases, [85]);
    assert_eq!(
        generated[1].facts.as_ref().unwrap().physical_group,
        Some(52_633)
    );
    assert_eq!(public["paths"][0]["score"], 216);
    assert_eq!(public["paths"][0]["tokens"][0]["candidateId"], 2);
    assert_eq!(public["paths"][0]["tokens"][0]["skipped"], 0);
    assert_eq!(public["paths"][0]["tokens"][0]["root"]["seq"], 1_519_210);

    let astral = "猫😀犬".encode_utf16().collect::<Vec<_>>();
    assert_eq!(
        kernel
            .surface()
            .scan(&astral, 0, 50)
            .unwrap()
            .last()
            .unwrap()
            .end,
        1
    );
    assert_eq!(
        kernel
            .surface()
            .scan(&astral, 3, 50)
            .unwrap()
            .last()
            .unwrap()
            .end,
        4
    );
    for malformed in [[0x732b, 0xd83d, 0x72ac], [0x732b, 0xde00, 0x72ac]] {
        assert_eq!(
            kernel
                .surface()
                .scan(&malformed, 0, 50)
                .unwrap()
                .last()
                .unwrap()
                .end,
            1
        );
        assert_eq!(
            kernel
                .surface()
                .scan(&malformed, 2, 50)
                .unwrap()
                .last()
                .unwrap()
                .end,
            3
        );
    }

    let astral_json =
        String::from_utf8(kernel.analyze_json(&[0xd83d, 0xde00], 1).unwrap()).unwrap();
    assert!(astral_json.contains("\"input\":\"😀\""));
    for (input, escaped) in [([0xd83d], "\\ud83d"), ([0xde00], "\\ude00")] {
        let serialized = String::from_utf8(kernel.analyze_json(&input, 1).unwrap()).unwrap();
        assert_eq!(serialized.matches(escaped).count(), 6);
        assert!(!serialized.contains('�'));
    }
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn detail_index_is_lazy_and_fetches_one_random_access_block() {
    let path = release().join("details.bin");
    let metadata = fs::metadata(&path).unwrap();
    assert_eq!(metadata.len(), 13_555_874);
    let mut file = File::open(path).unwrap();
    let mut header = vec![0_u8; 96];
    file.read_exact(&mut header).unwrap();
    let prefix_length = DetailStore::prefix_length(&header, metadata.len() as usize).unwrap();
    let mut prefix = vec![0_u8; prefix_length];
    file.seek(SeekFrom::Start(0)).unwrap();
    file.read_exact(&mut prefix).unwrap();
    let details = DetailStore::open(prefix, metadata.len() as usize).unwrap();
    assert_eq!(details.resident_bytes(), 1_755_112);
    let range = details.range(43_720).unwrap();
    assert_eq!((range.offset, range.byte_length), (4_017_644, 24_333));
    assert!(details.entry_cached(43_720).unwrap().is_none());
    let mut compressed = vec![0_u8; range.byte_length as usize];
    file.seek(SeekFrom::Start(range.offset as u64)).unwrap();
    file.read_exact(&mut compressed).unwrap();
    let entry = details.entry_from_compressed(43_720, &compressed).unwrap();
    assert_eq!(entry.seq, 1_467_640);
    assert!(details.entry_cached(43_720).unwrap().is_some());
}
