use std::error::Error;
use std::fs;
use std::io::{self, Read};

use ichiran_kernel::{AnalysisResult, AnalyzeOptions, EntityHint, Kernel};
use serde::Deserialize;

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
    #[serde(default = "default_limit")]
    limit: usize,
    #[serde(default)]
    entities: Vec<BatchEntity>,
    #[serde(default)]
    normalize_punctuation: bool,
}

fn default_limit() -> usize {
    5
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut arguments = std::env::args_os();
    let executable = arguments.next().unwrap_or_default();
    let hot_path = arguments.next().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("usage: {} <hot.bin>", executable.to_string_lossy()),
        )
    })?;
    if arguments.next().is_some() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("usage: {} <hot.bin>", executable.to_string_lossy()),
        )
        .into());
    }

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let requests: Vec<BatchRequest> = serde_json::from_str(&input)?;
    let hot = fs::read(hot_path)?;
    let mut kernel = Kernel::open(hot)?;
    let mut results = Vec::<AnalysisResult>::with_capacity(requests.len());
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
        results.push(
            kernel
                .analyze_with_options(&request.text.encode_utf16().collect::<Vec<_>>(), &options)
                .map_err(|error| io::Error::other(format!("request {index}: {error}")))?,
        );
    }
    serde_json::to_writer(io::stdout().lock(), &results)?;
    Ok(())
}
