use std::fs;

use ichiran_kernel::{ErrorCode, Kernel};

const DIRECTORY_START: usize = 32;
const DIRECTORY_ENTRY_BYTES: usize = 24;

fn u32(bytes: &[u8], at: usize) -> usize {
    u32::from_le_bytes(bytes[at..at + 4].try_into().unwrap()) as usize
}

fn put_u32(bytes: &mut [u8], at: usize, value: usize) {
    bytes[at..at + 4].copy_from_slice(&(value as u32).to_le_bytes());
}

fn qualified_hot() -> Vec<u8> {
    let directory = std::env::var("ICHIRAN_M1_PACK_DIR").expect("qualified release directory");
    fs::read(format!("{directory}/hot.bin")).expect("qualified hot.bin")
}

fn morphology(bytes: &[u8]) -> (usize, usize) {
    let sections = u32(bytes, 16);
    for index in 0..sections {
        let entry = DIRECTORY_START + index * DIRECTORY_ENTRY_BYTES;
        if u32(bytes, entry) == 3 {
            return (entry, u32(bytes, entry + 4));
        }
    }
    panic!("morphology section is missing")
}

fn expect_corrupt(mut bytes: Vec<u8>, mutate: impl FnOnce(&mut [u8])) {
    let (entry, start) = morphology(&bytes);
    let length = u32(&bytes, entry + 8);
    mutate(&mut bytes[start..start + length]);
    let section_crc = crc32fast::hash(&bytes[start..start + length]);
    put_u32(&mut bytes, entry + 12, section_crc as usize);
    let directory_bytes = u32(&bytes, 20);
    let directory_crc = crc32fast::hash(&bytes[DIRECTORY_START..DIRECTORY_START + directory_bytes]);
    put_u32(&mut bytes, 28, directory_crc as usize);
    let error = Kernel::open(bytes)
        .err()
        .expect("corrupt morphology was accepted");
    assert_eq!(error.code, ErrorCode::CorruptPayload, "{}", error.message);
}

fn duplicate_in_first_non_singleton_bucket(
    section: &mut [u8],
    bucket: [usize; 5],
    records: [usize; 2],
) {
    let [
        buckets_at,
        buckets,
        bucket_stride,
        count_offset,
        count_width,
    ] = bucket;
    let [records_at, record_stride] = records;
    for bucket in 0..buckets {
        let at = buckets_at + bucket * bucket_stride;
        let first = u32(section, at + 4);
        let count = if count_width == 2 {
            u16::from_le_bytes(
                section[at + count_offset..at + count_offset + 2]
                    .try_into()
                    .unwrap(),
            ) as usize
        } else {
            u32(section, at + count_offset)
        };
        if count >= 2 {
            let first_at = records_at + first * record_stride;
            section.copy_within(first_at..first_at + record_stride, first_at + record_stride);
            return;
        }
    }
    panic!("qualified morphology has no non-singleton bucket")
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn validates_all_deferred_morphology_records_at_open() {
    let hot = qualified_hot();
    expect_corrupt(hot.clone(), |section| {
        let root_record = u32(section, 88);
        put_u32(section, root_record, u32(section, 44));
    });
    expect_corrupt(hot.clone(), |section| {
        let template = u32(section, 80);
        put_u32(section, template, u32(section, 60));
    });
    expect_corrupt(hot.clone(), |section| {
        let patch = u32(section, 108);
        put_u32(section, patch + 4, u32(section, 60));
    });
    expect_corrupt(hot, |section| {
        let tombstone = u32(section, 124);
        put_u32(section, tombstone + 8, u32(section, 20));
    });
}

#[test]
#[ignore = "requires the digest-locked portable-core-260118-baseline release"]
fn rejects_noncanonical_morphology_record_order() {
    let hot = qualified_hot();
    expect_corrupt(hot.clone(), |section| {
        duplicate_in_first_non_singleton_bucket(
            section,
            [u32(section, 76), u32(section, 24), 12, 8, 4],
            [u32(section, 80), 12],
        );
    });
    expect_corrupt(hot.clone(), |section| {
        duplicate_in_first_non_singleton_bucket(
            section,
            [u32(section, 84), u32(section, 32), 16, 8, 4],
            [u32(section, 88), 16],
        );
    });
    expect_corrupt(hot.clone(), |section| {
        assert!(u32(section, 52) >= 2);
        let buckets = u32(section, 104);
        section.copy_within(buckets..buckets + 4, buckets + 12);
        section[buckets + 22] = section[buckets + 10];
    });
    expect_corrupt(hot, |section| {
        assert!(u32(section, 120) >= 2);
        let tombstones = u32(section, 124);
        section.copy_within(tombstones..tombstones + 20, tombstones + 20);
    });
}
