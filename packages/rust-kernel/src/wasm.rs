use wasm_bindgen::prelude::*;

use crate::{DetailStore, Kernel, KernelError, Route};

#[wasm_bindgen]
pub struct WasmKernel {
    inner: Kernel,
}

#[wasm_bindgen]
impl WasmKernel {
    #[wasm_bindgen(constructor)]
    pub fn open(hot: Vec<u8>) -> std::result::Result<WasmKernel, JsValue> {
        Kernel::open(hot)
            .map(|inner| Self { inner })
            .map_err(js_error)
    }

    /// The only analysis crossing: one UTF-16 input and one UTF-8 JSON result.
    pub fn analyze_utf16(
        &mut self,
        input: &[u16],
        limit: u32,
    ) -> std::result::Result<Vec<u8>, JsValue> {
        self.inner
            .analyze_json(input, limit as usize)
            .map_err(js_error)
    }

    pub fn inspect_generated_utf16(
        &mut self,
        input: &[u16],
        kanji_route: bool,
    ) -> std::result::Result<Vec<u8>, JsValue> {
        let route = if kanji_route {
            Route::Kanji
        } else {
            Route::Kana
        };
        let value = self
            .inner
            .generated_lookup(input, route)
            .map_err(js_error)?;
        serde_json::to_vec(&value).map_err(|error| JsValue::from_str(&error.to_string()))
    }

    pub fn resident_payload_bytes(&self) -> u32 {
        self.inner.resident_payload_bytes() as u32
    }
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
