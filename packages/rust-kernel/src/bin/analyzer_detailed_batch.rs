use std::collections::HashSet;
use std::error::Error;
use std::fs::{self, File};
use std::io::{self, Read, Seek, SeekFrom};

use ichiran_kernel::{
    AnalyzeOptions, DictionaryStoreKind, DictionaryStores, EntityHint, Kernel, LegacyDetailSession,
    LegacyDetailStep, LexiconStore, LocaleStore,
};
use serde::Deserialize;
use serde_json::Value;

const LEXICON_HEADER_BYTES: usize = 96;
const LOCALE_HEADER_BYTES: usize = 128;

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
            "usage: {} <hot.bin> <lexicon.bin> <gloss.bin> <locale> <lexicon-sha256>",
            executable.to_string_lossy()
        ),
    )
}

fn open_lexicon(path: &std::ffi::OsStr) -> Result<(File, LexiconStore), Box<dyn Error>> {
    let mut file = File::open(path)?;
    let total_bytes = usize::try_from(file.metadata()?.len())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "lexicon.bin is too large"))?;
    let mut prefix = vec![0; LEXICON_HEADER_BYTES];
    file.read_exact(&mut prefix)?;
    let prefix_length = LexiconStore::prefix_length(&prefix, total_bytes)?;
    prefix.resize(prefix_length, 0);
    file.read_exact(&mut prefix[LEXICON_HEADER_BYTES..])?;
    let store = LexiconStore::open(prefix, total_bytes)?;
    Ok((file, store))
}

fn open_locale(
    path: &std::ffi::OsStr,
    locale: &str,
    digest: &[u8],
    entry_count: usize,
) -> Result<(File, LocaleStore), Box<dyn Error>> {
    let mut file = File::open(path)?;
    let total_bytes = usize::try_from(file.metadata()?.len())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "gloss.bin is too large"))?;
    let mut prefix = vec![0; LOCALE_HEADER_BYTES];
    file.read_exact(&mut prefix)?;
    let prefix_length = LocaleStore::prefix_length(&prefix, total_bytes)?;
    prefix.resize(prefix_length, 0);
    file.read_exact(&mut prefix[LOCALE_HEADER_BYTES..])?;
    let store = LocaleStore::open(prefix, total_bytes, digest, locale, entry_count)?;
    Ok((file, store))
}

fn decode_hex(value: &str) -> Result<Vec<u8>, Box<dyn Error>> {
    if value.len() != 64 {
        return Err(usage(std::ffi::OsStr::new("analyzer_detailed_batch")).into());
    }
    (0..32)
        .map(|index| u8::from_str_radix(&value[index * 2..index * 2 + 2], 16).map_err(Into::into))
        .collect()
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut arguments = std::env::args_os();
    let executable = arguments.next().unwrap_or_default();
    let hot_path = arguments.next().ok_or_else(|| usage(&executable))?;
    let lexicon_path = arguments.next().ok_or_else(|| usage(&executable))?;
    let locale_path = arguments.next().ok_or_else(|| usage(&executable))?;
    let locale = arguments
        .next()
        .ok_or_else(|| usage(&executable))?
        .into_string()
        .map_err(|_| usage(&executable))?;
    let digest = decode_hex(
        &arguments
            .next()
            .ok_or_else(|| usage(&executable))?
            .into_string()
            .map_err(|_| usage(&executable))?,
    )?;
    if arguments.next().is_some() {
        return Err(usage(&executable).into());
    }

    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let requests: Vec<BatchRequest> = serde_json::from_str(&input)?;
    let mut kernel = Kernel::open(fs::read(hot_path)?)?;
    let (mut lexicon_file, lexicon) = open_lexicon(&lexicon_path)?;
    let (mut locale_file, locale_store) =
        open_locale(&locale_path, &locale, &digest, lexicon.entry_count())?;
    let stores = DictionaryStores {
        lexicon: &lexicon,
        locale: &locale_store,
        fallback: &locale_store,
    };
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
                .serialize_legacy_detailed_json(&mut session, &analysis, &stores, None)
                .map_err(|error| {
                    io::Error::other(format!("request {index} serialization: {error}"))
                })? {
                LegacyDetailStep::Ready(json) => break serde_json::from_slice(&json)?,
                LegacyDetailStep::Missing {
                    store,
                    entry_index,
                    range,
                } => {
                    if !requested.insert((store, entry_index)) {
                        return Err(io::Error::other(format!(
                            "request {index} repeated detail entry {entry_index}"
                        ))
                        .into());
                    }
                    let mut compressed = vec![0; range.byte_length as usize];
                    let file = match store {
                        DictionaryStoreKind::Lexicon => &mut lexicon_file,
                        DictionaryStoreKind::Locale | DictionaryStoreKind::Fallback => {
                            &mut locale_file
                        }
                    };
                    file.seek(SeekFrom::Start(u64::from(range.offset)))?;
                    file.read_exact(&mut compressed)?;
                    match store {
                        DictionaryStoreKind::Lexicon => lexicon
                            .entry_from_compressed(entry_index, &compressed)
                            .map(|_| ()),
                        DictionaryStoreKind::Locale | DictionaryStoreKind::Fallback => locale_store
                            .entry_from_compressed(entry_index, &compressed)
                            .map(|_| ()),
                    }
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
