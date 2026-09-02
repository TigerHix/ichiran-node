use std::collections::HashSet;
use std::error::Error;
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom};

use ichiran_kernel::{
    AnalyzeOptions, DetailStore, EntityHint, Kernel, LegacyDetailSession, LegacyDetailStep,
};
use serde::Deserialize;
use serde_json::Value;

const DETAIL_HEADER_BYTES: usize = 96;

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BatchEntity {
    start: usize,
    end: usize,
    #[serde(default)]
    boost: Option<f64>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct BatchRequest {
    text: String,
    limit: usize,
    #[serde(default)]
    entities: Vec<BatchEntity>,
    normalize_punctuation: bool,
}

fn usage(executable: &std::ffi::OsStr) -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidInput,
        format!(
            "usage: {} <hot.bin> <details.bin>",
            executable.to_string_lossy()
        ),
    )
}

fn open_details(path: &std::ffi::OsStr) -> Result<(File, DetailStore), Box<dyn Error>> {
    let mut file = File::open(path)?;
    let total_bytes = usize::try_from(file.metadata()?.len())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "details.bin is too large"))?;
    let mut prefix = vec![0; DETAIL_HEADER_BYTES];
    file.read_exact(&mut prefix)?;
    let prefix_length = DetailStore::prefix_length(&prefix, total_bytes)?;
    prefix.resize(prefix_length, 0);
    file.read_exact(&mut prefix[DETAIL_HEADER_BYTES..])?;
    let store = DetailStore::open(prefix, total_bytes)?;
    Ok((file, store))
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut arguments = std::env::args_os();
    let executable = arguments.next().unwrap_or_default();
    let hot_path = arguments.next().ok_or_else(|| usage(&executable))?;
    let details_path = arguments.next().ok_or_else(|| usage(&executable))?;
    if arguments.next().is_some() {
        return Err(usage(&executable).into());
    }

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let requests: Vec<BatchRequest> = serde_json::from_str(&input)?;
    let mut kernel = Kernel::open(fs::read(hot_path)?)?;
    let (mut details_file, details) = open_details(&details_path)?;
    let mut results = Vec::<Value>::with_capacity(requests.len());

    for (index, request) in requests.into_iter().enumerate() {
        let options = AnalyzeOptions {
            limit: request.limit,
            entities: request
                .entities
                .into_iter()
                .map(|entity| EntityHint {
                    start: entity.start,
                    end: entity.end,
                    boost: entity.boost,
                })
                .collect(),
            normalize_punctuation: request.normalize_punctuation,
        };
        let analysis = kernel
            .analyze_with_options(&request.text.encode_utf16().collect::<Vec<_>>(), &options)
            .map_err(|error| io::Error::other(format!("request {index} analysis: {error}")))?;
        let mut session = LegacyDetailSession::default();
        let mut requested = HashSet::new();
        let detailed = loop {
            match kernel
                .serialize_legacy_detailed_json(&mut session, &analysis, &details, None)
                .map_err(|error| {
                    io::Error::other(format!("request {index} serialization: {error}"))
                })? {
                LegacyDetailStep::Ready(json) => break serde_json::from_slice(&json)?,
                LegacyDetailStep::Missing { entry_index, range } => {
                    if !requested.insert(entry_index) {
                        return Err(io::Error::other(format!(
                            "request {index} repeated detail entry {entry_index}"
                        ))
                        .into());
                    }
                    let mut compressed = vec![0; range.byte_length as usize];
                    details_file.seek(SeekFrom::Start(u64::from(range.offset)))?;
                    details_file.read_exact(&mut compressed)?;
                    details
                        .entry_from_compressed(entry_index, &compressed)
                        .map_err(|error| {
                            io::Error::other(format!(
                                "request {index} detail entry {entry_index}: {error}"
                            ))
                        })?;
                }
            }
        };
        results.push(detailed);
    }

    serde_json::to_writer(io::stdout().lock(), &results)?;
    Ok(())
}
