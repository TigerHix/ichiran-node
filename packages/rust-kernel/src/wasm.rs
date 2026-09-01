use wasm_bindgen::prelude::*;

use serde::{Deserialize, Serialize};

use crate::analysis::LegacyWireDetailStep;
use crate::{
    AnalyzeOptions, DetailRange, DetailStore, EntityHint, ErrorCode, Kernel, KernelError,
    LegacyDetailSession, RomanizationName,
};

struct WasmLegacyOperation {
    analysis: crate::AnalysisResult,
    session: LegacyDetailSession,
    method: Option<RomanizationName>,
}

#[wasm_bindgen]
pub struct WasmKernel {
    inner: Kernel,
    legacy: Option<WasmLegacyOperation>,
}

#[wasm_bindgen]
impl WasmKernel {
    #[wasm_bindgen(constructor)]
    pub fn open(hot: Vec<u8>) -> std::result::Result<WasmKernel, JsValue> {
        Kernel::open(hot)
            .map(|inner| Self {
                inner,
                legacy: None,
            })
            .map_err(js_error)
    }

    /// The only analysis crossing: one UTF-16 input, one options document, and
    /// one UTF-8 JSON result.
    pub fn analyze_utf16_options(
        &mut self,
        input: &[u16],
        options_json: &[u8],
    ) -> std::result::Result<Vec<u8>, JsValue> {
        let options = parse_options(options_json).map_err(js_error)?;
        self.inner
            .analyze_json_with_options(input, &options)
            .map_err(js_error)
    }

    pub fn romanize_utf16_options(
        &mut self,
        input: &[u16],
        options_json: &[u8],
        method: &str,
    ) -> std::result::Result<Vec<u16>, JsValue> {
        let options = parse_options(options_json).map_err(js_error)?;
        let method = parse_method(method)
            .map_err(js_error)?
            .unwrap_or(RomanizationName::HepburnTraditional);
        self.inner
            .romanize_with_options(input, &options, method)
            .map_err(js_error)
    }

    pub fn legacy_begin_utf16(
        &mut self,
        input: &[u16],
        options_json: &[u8],
        method: &str,
    ) -> std::result::Result<(), JsValue> {
        let options = parse_options(options_json).map_err(js_error)?;
        let method = parse_method(method).map_err(js_error)?;
        let analysis = self
            .inner
            .analyze_with_options(input, &options)
            .map_err(js_error)?;
        self.legacy = Some(WasmLegacyOperation {
            analysis,
            session: LegacyDetailSession::default(),
            method,
        });
        Ok(())
    }

    /// Returns a JSON envelope. `missing-detail` names the exact compressed
    /// range the host must feed to `WasmDetailStore.entry_json` before retrying.
    pub fn legacy_step(
        &mut self,
        details: &WasmDetailStore,
    ) -> std::result::Result<Vec<u8>, JsValue> {
        let operation = self.legacy.as_mut().ok_or_else(|| {
            js_error(KernelError::new(
                ErrorCode::InvalidInput,
                "legacy_begin_utf16 must precede legacy_step",
            ))
        })?;
        match self
            .inner
            .serialize_legacy_detailed_wire_json(
                &mut operation.session,
                &operation.analysis,
                &details.inner,
                operation.method,
            )
            .map_err(js_error)?
        {
            LegacyWireDetailStep::Ready { value, metadata } => {
                self.legacy = None;
                let mut envelope = Vec::with_capacity(value.len() + metadata.len() + 39);
                envelope.extend_from_slice(b"{\"state\":\"ready\",\"value\":");
                envelope.extend_from_slice(&value);
                envelope.extend_from_slice(b",\"metadata\":");
                envelope.extend_from_slice(&metadata);
                envelope.push(b'}');
                Ok(envelope)
            }
            LegacyWireDetailStep::Missing { entry_index, range } => {
                serde_json::to_vec(&WasmMissingDetail {
                    state: "missing-detail",
                    entry_index,
                    range,
                })
                .map_err(|error| JsValue::from_str(&error.to_string()))
            }
        }
    }

    pub fn resident_payload_bytes(&self) -> u32 {
        self.inner.resident_payload_bytes() as u32
    }

    pub fn entry_index_for_sequence(&self, sequence: u32) -> std::result::Result<i32, JsValue> {
        self.inner
            .entry_index_for_sequence(sequence)
            .and_then(|value| {
                value.map_or(Ok(-1), |index| {
                    i32::try_from(index).map_err(|_| {
                        KernelError::new(ErrorCode::OutOfRange, "detail entry index exceeds int32")
                    })
                })
            })
            .map_err(js_error)
    }
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct WasmAnalyzeOptions {
    limit: usize,
    entities: Vec<WasmEntityHint>,
    normalize_punctuation: bool,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct WasmEntityHint {
    start: usize,
    end: usize,
    boost: Option<f64>,
}

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct WasmMissingDetail {
    state: &'static str,
    entry_index: u32,
    range: DetailRange,
}

fn parse_options(json: &[u8]) -> crate::Result<AnalyzeOptions> {
    let options: WasmAnalyzeOptions = serde_json::from_slice(json).map_err(|error| {
        KernelError::new(
            ErrorCode::InvalidInput,
            format!("analysis options are invalid JSON: {error}"),
        )
    })?;
    Ok(AnalyzeOptions {
        limit: options.limit,
        entities: options
            .entities
            .into_iter()
            .map(|entity| EntityHint {
                start: entity.start,
                end: entity.end,
                boost: entity.boost,
            })
            .collect(),
        normalize_punctuation: options.normalize_punctuation,
    })
}

fn parse_method(value: &str) -> crate::Result<Option<RomanizationName>> {
    let method = match value {
        "" => return Ok(None),
        "hepburn-basic" => RomanizationName::HepburnBasic,
        "hepburn-simple" => RomanizationName::HepburnSimple,
        "hepburn-passport" => RomanizationName::HepburnPassport,
        "hepburn-traditional" => RomanizationName::HepburnTraditional,
        "hepburn-modified" => RomanizationName::HepburnModified,
        "kunrei-siki" => RomanizationName::KunreiSiki,
        _ => {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                "romanization method is not supported",
            ));
        }
    };
    Ok(Some(method))
}

fn js_error(error: KernelError) -> JsValue {
    let code = serde_json::to_value(error.code)
        .ok()
        .and_then(|value| value.as_str().map(ToOwned::to_owned))
        .unwrap_or_else(|| "internal".to_owned());
    let value = js_sys::Error::new(&error.message);
    value.set_name("RustKernelError");
    let _ = js_sys::Reflect::set(
        value.as_ref(),
        &JsValue::from_str("code"),
        &JsValue::from_str(&code),
    );
    value.into()
}

#[wasm_bindgen]
pub fn detail_prefix_length(header: &[u8], total_bytes: u32) -> std::result::Result<u32, JsValue> {
    DetailStore::prefix_length(header, total_bytes as usize)
        .and_then(|length| {
            u32::try_from(length).map_err(|_| {
                KernelError::new(crate::ErrorCode::OutOfRange, "detail prefix exceeds uint32")
            })
        })
        .map_err(js_error)
}

#[wasm_bindgen]
pub struct WasmDetailStore {
    inner: DetailStore,
}

#[wasm_bindgen]
impl WasmDetailStore {
    #[wasm_bindgen(constructor)]
    pub fn open(prefix: &[u8], total_bytes: u32) -> std::result::Result<WasmDetailStore, JsValue> {
        DetailStore::open(prefix.to_vec(), total_bytes as usize)
            .map(|inner| Self { inner })
            .map_err(js_error)
    }

    pub fn range_json(&self, entry_index: u32) -> std::result::Result<Vec<u8>, JsValue> {
        let range = self.inner.range(entry_index).map_err(js_error)?;
        serde_json::to_vec(&range).map_err(|error| JsValue::from_str(&error.to_string()))
    }

    pub fn entry_json(
        &self,
        entry_index: u32,
        compressed: &[u8],
    ) -> std::result::Result<Vec<u8>, JsValue> {
        let entry = self
            .inner
            .entry_from_compressed(entry_index, compressed)
            .map_err(js_error)?;
        serde_json::to_vec(&entry).map_err(|error| JsValue::from_str(&error.to_string()))
    }

    pub fn resident_bytes(&self) -> u32 {
        self.inner.resident_bytes() as u32
    }
}
