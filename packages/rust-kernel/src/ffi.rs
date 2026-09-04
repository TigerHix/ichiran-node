use std::mem::{align_of, size_of};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::ptr;
use std::slice;
use std::sync::Mutex;

use serde::{Deserialize, Serialize};

use crate::{
    AnalyzeOptions, DictionaryRange, DictionaryStoreKind, DictionaryStores, EntityHint, ErrorCode,
    Kernel, KernelError, LegacyDetailSession, LegacyDetailStep, LexiconStore, LocaleStore, Result,
    RomanizationName, TokenDetailsSession, TokenDetailsStep, Utf16Text,
};

const ABI_VERSION: u32 = 7;
const NO_DICTIONARY: u32 = u32::MAX;
const DICTIONARY_NONE: u32 = 0;

#[repr(C)]
pub struct IchiranBuffer {
    pub data: *mut u8,
    pub byte_length: usize,
    pub capacity: usize,
}

impl IchiranBuffer {
    fn from_vec(mut bytes: Vec<u8>) -> Self {
        let result = Self {
            data: bytes.as_mut_ptr(),
            byte_length: bytes.len(),
            capacity: bytes.capacity(),
        };
        std::mem::forget(bytes);
        result
    }
}

#[repr(C)]
pub struct IchiranResult {
    pub status: u32,
    pub buffer: IchiranBuffer,
}

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct IchiranDictionaryRange {
    pub block: u32,
    pub offset: u32,
    pub byte_length: u32,
    pub uncompressed_bytes: u32,
    pub checksum: u32,
}

impl From<DictionaryRange> for IchiranDictionaryRange {
    fn from(value: DictionaryRange) -> Self {
        Self {
            block: value.block,
            offset: value.offset,
            byte_length: value.byte_length,
            uncompressed_bytes: value.uncompressed_bytes,
            checksum: value.checksum,
        }
    }
}

#[repr(C)]
pub struct IchiranStepResult {
    pub status: u32,
    pub state: u32,
    pub store: u32,
    pub entry_index: u32,
    pub range: IchiranDictionaryRange,
    pub buffer: IchiranBuffer,
}

pub struct IchiranKernel {
    inner: Mutex<Kernel>,
}

pub struct IchiranLexiconStore {
    inner: Mutex<LexiconStore>,
}

pub struct IchiranLocaleStore {
    inner: Mutex<LocaleStore>,
}

struct LegacyOperationState {
    analysis: crate::AnalysisResult,
    session: LegacyDetailSession,
    method: Option<RomanizationName>,
    pending: Option<(DictionaryStoreKind, u32, DictionaryRange)>,
    completed: bool,
}

pub struct IchiranLegacyOperation {
    inner: Mutex<LegacyOperationState>,
}

struct TokenDetailsOperationState {
    analysis: crate::AnalysisResult,
    session: TokenDetailsSession,
    path_index: usize,
    token_index: usize,
    pending: Option<(DictionaryStoreKind, u32, DictionaryRange)>,
    completed: bool,
}

pub struct IchiranTokenDetailsOperation {
    inner: Mutex<TokenDetailsOperationState>,
}

#[derive(Serialize)]
struct ErrorBody<'a> {
    code: ErrorCode,
    message: &'a str,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
struct CAnalyzeOptions {
    limit: usize,
    entities: Vec<CEntityHint>,
    normalize_punctuation: bool,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CEntityHint {
    start: usize,
    end: usize,
    boost: Option<f64>,
}

#[unsafe(no_mangle)]
pub extern "C" fn ichiran_kernel_abi_version() -> u32 {
    ABI_VERSION
}

#[unsafe(no_mangle)]
/// Opens a kernel from one complete hot pack.
///
/// # Safety
///
/// `output` must point to writable storage for one kernel pointer. When
/// `hot_bytes` is nonzero, `hot` must point to that many readable bytes for the
/// duration of this call. The returned kernel must be freed exactly once.
pub unsafe extern "C" fn ichiran_kernel_open(
    hot: *const u8,
    hot_bytes: usize,
    output: *mut *mut IchiranKernel,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "kernel output")?;
        unsafe {
            *output = ptr::null_mut();
        }
        let bytes = input_bytes(hot, hot_bytes, "hot pack")?.to_vec();
        let kernel = Box::new(IchiranKernel {
            inner: Mutex::new(Kernel::open(bytes)?),
        });
        unsafe {
            *output = Box::into_raw(kernel);
        }
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
/// Executes one analysis over UTF-16 code units.
///
/// # Safety
///
/// `kernel` must be a live handle returned by `ichiran_kernel_open` and must
/// remain live until this call returns. When `input_units` is nonzero, `input`
/// must point to that many aligned, readable `u16` values for the duration of
/// this call. When `options_bytes` is nonzero, `options_json` must point to that
/// many readable UTF-8 JSON bytes for the duration of this call.
pub unsafe extern "C" fn ichiran_kernel_analyze_utf16(
    kernel: *const IchiranKernel,
    input: *const u16,
    input_units: usize,
    options_json: *const u8,
    options_bytes: usize,
) -> IchiranResult {
    boundary(|| {
        let kernel = unsafe { handle(kernel, "kernel")? };
        let input = input_units_slice(input, input_units)?;
        let options = parse_options(input_bytes(
            options_json,
            options_bytes,
            "analysis options",
        )?)?;
        let mut analyzer = kernel.inner.lock().map_err(|_| {
            KernelError::new(
                ErrorCode::Internal,
                "kernel is unavailable after an earlier analysis panic",
            )
        })?;
        analyzer.analyze_json_with_options(input, &options)
    })
}

#[unsafe(no_mangle)]
/// Executes standalone romanization and returns one lossless JSON string.
///
/// # Safety
///
/// All pointers are borrowed only for this call. `kernel` must be live,
/// `input` must meet the requirements of `ichiran_kernel_analyze_utf16`, and
/// non-empty byte inputs must point to readable storage.
pub unsafe extern "C" fn ichiran_kernel_romanize_utf16(
    kernel: *const IchiranKernel,
    input: *const u16,
    input_units: usize,
    options_json: *const u8,
    options_bytes: usize,
    method_utf8: *const u8,
    method_bytes: usize,
) -> IchiranResult {
    boundary(|| {
        let kernel = unsafe { handle(kernel, "kernel")? };
        let input = input_units_slice(input, input_units)?;
        let options = parse_options(input_bytes(
            options_json,
            options_bytes,
            "analysis options",
        )?)?;
        let method = parse_method(input_bytes(
            method_utf8,
            method_bytes,
            "romanization method",
        )?)?
        .unwrap_or(RomanizationName::HepburnTraditional);
        let mut analyzer = lock(&kernel.inner, "kernel")?;
        let value = analyzer.romanize_with_options(input, &options, method)?;
        serialize(&Utf16Text::from_units(&value))
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_lexicon_prefix_length(
    header: *const u8,
    header_bytes: usize,
    total_bytes: usize,
    output: *mut usize,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "lexicon prefix-length output")?;
        unsafe { *output = 0 };
        let length = LexiconStore::prefix_length(
            input_bytes(header, header_bytes, "lexicon header")?,
            total_bytes,
        )?;
        unsafe { *output = length };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_locale_prefix_length(
    header: *const u8,
    header_bytes: usize,
    total_bytes: usize,
    output: *mut usize,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "locale prefix-length output")?;
        unsafe { *output = 0 };
        let length = LocaleStore::prefix_length(
            input_bytes(header, header_bytes, "locale header")?,
            total_bytes,
        )?;
        unsafe { *output = length };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_lexicon_store_open(
    prefix: *const u8,
    prefix_bytes: usize,
    total_bytes: usize,
    output: *mut *mut IchiranLexiconStore,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "lexicon-store output")?;
        unsafe { *output = ptr::null_mut() };
        let store = LexiconStore::open(
            input_bytes(prefix, prefix_bytes, "lexicon prefix")?.to_vec(),
            total_bytes,
        )?;
        let store = Box::new(IchiranLexiconStore {
            inner: Mutex::new(store),
        });
        unsafe { *output = Box::into_raw(store) };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
/// Returns the exact compressed range needed for one entry.
///
/// # Safety
///
/// `lexicon` must be live and `output` must be writable for one range.
pub unsafe extern "C" fn ichiran_lexicon_store_entry_count(
    lexicon: *const IchiranLexiconStore,
) -> usize {
    unsafe { lexicon.as_ref() }.map_or(0, |store| {
        store.inner.lock().map_or(0, |inner| inner.entry_count())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_lexicon_store_range(
    lexicon: *const IchiranLexiconStore,
    entry_index: u32,
    output: *mut IchiranDictionaryRange,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "dictionary-range output")?;
        unsafe { *output = IchiranDictionaryRange::default() };
        let lexicon = unsafe { handle(lexicon, "lexicon store")? };
        let store = lock(&lexicon.inner, "lexicon store")?;
        unsafe { *output = store.range(entry_index)?.into() };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
/// Decodes one host-supplied compressed lexicon block and returns entry JSON.
///
/// # Safety
///
/// `lexicon` must be live. `compressed` is borrowed only for this call.
pub unsafe extern "C" fn ichiran_lexicon_store_decode(
    lexicon: *const IchiranLexiconStore,
    entry_index: u32,
    compressed: *const u8,
    compressed_bytes: usize,
) -> IchiranResult {
    boundary(|| {
        let lexicon = unsafe { handle(lexicon, "lexicon store")? };
        let compressed = input_bytes(compressed, compressed_bytes, "compressed lexicon block")?;
        let store = lock(&lexicon.inner, "lexicon store")?;
        serialize(&store.entry_from_compressed(entry_index, compressed)?)
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_locale_store_open(
    prefix: *const u8,
    prefix_bytes: usize,
    total_bytes: usize,
    lexicon_sha256: *const u8,
    locale_utf8: *const u8,
    locale_bytes: usize,
    lexicon_entry_count: usize,
    output: *mut *mut IchiranLocaleStore,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "locale-store output")?;
        unsafe { *output = ptr::null_mut() };
        let digest = input_bytes(lexicon_sha256, 32, "lexicon SHA-256")?;
        let locale = std::str::from_utf8(input_bytes(locale_utf8, locale_bytes, "locale")?)
            .map_err(|_| KernelError::new(ErrorCode::InvalidInput, "locale is not valid UTF-8"))?;
        let store = LocaleStore::open(
            input_bytes(prefix, prefix_bytes, "locale prefix")?.to_vec(),
            total_bytes,
            digest,
            locale,
            lexicon_entry_count,
        )?;
        unsafe {
            *output = Box::into_raw(Box::new(IchiranLocaleStore {
                inner: Mutex::new(store),
            }))
        };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_locale_store_range(
    locale: *const IchiranLocaleStore,
    entry_index: u32,
    output: *mut IchiranDictionaryRange,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "dictionary-range output")?;
        unsafe { *output = IchiranDictionaryRange::default() };
        let locale = unsafe { handle(locale, "locale store")? };
        let store = lock(&locale.inner, "locale store")?;
        unsafe { *output = store.range(entry_index)?.into() };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_locale_store_decode(
    locale: *const IchiranLocaleStore,
    entry_index: u32,
    compressed: *const u8,
    compressed_bytes: usize,
) -> IchiranResult {
    boundary(|| {
        let locale = unsafe { handle(locale, "locale store")? };
        let compressed = input_bytes(compressed, compressed_bytes, "compressed locale block")?;
        let store = lock(&locale.inner, "locale store")?;
        serialize(&store.entry_from_compressed(entry_index, compressed)?)
    })
}

#[unsafe(no_mangle)]
/// Begins one independent retained detailed/legacy operation.
///
/// # Safety
///
/// Inputs are borrowed only for this call. `output` must be writable for one
/// operation pointer and the returned operation must be freed exactly once.
pub unsafe extern "C" fn ichiran_kernel_legacy_begin_utf16(
    kernel: *const IchiranKernel,
    input: *const u16,
    input_units: usize,
    options_json: *const u8,
    options_bytes: usize,
    method_utf8: *const u8,
    method_bytes: usize,
    output: *mut *mut IchiranLegacyOperation,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "legacy-operation output")?;
        unsafe { *output = ptr::null_mut() };
        let kernel = unsafe { handle(kernel, "kernel")? };
        let input = input_units_slice(input, input_units)?;
        let options = parse_options(input_bytes(
            options_json,
            options_bytes,
            "analysis options",
        )?)?;
        let method = parse_method(input_bytes(
            method_utf8,
            method_bytes,
            "romanization method",
        )?)?;
        let analysis = lock(&kernel.inner, "kernel")?.analyze_with_options(input, &options)?;
        let operation = Box::new(IchiranLegacyOperation {
            inner: Mutex::new(LegacyOperationState {
                analysis,
                session: LegacyDetailSession::default(),
                method,
                pending: None,
                completed: false,
            }),
        });
        unsafe { *output = Box::into_raw(operation) };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
/// Advances one detailed/legacy operation by at most one dictionary block read.
///
/// Pass `ICHIRAN_DICTIONARY_NONE`, `ICHIRAN_NO_DICTIONARY`, NULL, and zero on
/// the first call. After a missing result, pass exactly the returned store,
/// entry index, and compressed range. Decode and retry are atomic.
///
/// # Safety
///
/// All handles must be live and must not be freed during this call. A supplied
/// compressed block is borrowed only for this call.
pub unsafe extern "C" fn ichiran_kernel_legacy_step(
    kernel: *const IchiranKernel,
    operation: *const IchiranLegacyOperation,
    lexicon: *const IchiranLexiconStore,
    locale: *const IchiranLocaleStore,
    fallback: *const IchiranLocaleStore,
    supplied_store: u32,
    supplied_entry_index: u32,
    compressed: *const u8,
    compressed_bytes: usize,
) -> IchiranStepResult {
    step_boundary(|| {
        let kernel = unsafe { handle(kernel, "kernel")? };
        let operation = unsafe { handle(operation, "legacy operation")? };
        let lexicon = unsafe { handle(lexicon, "lexicon store")? };
        let locale = unsafe { handle(locale, "locale store")? };
        let fallback = unsafe { handle(fallback, "fallback locale store")? };
        let mut operation = lock(&operation.inner, "legacy operation")?;
        if operation.completed {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                "legacy operation is already complete",
            ));
        }
        if compressed_bytes == 0 {
            if supplied_store != DICTIONARY_NONE
                || supplied_entry_index != NO_DICTIONARY
                || !compressed.is_null()
            {
                return Err(KernelError::new(
                    ErrorCode::InvalidInput,
                    "an empty legacy step must not supply a dictionary entry",
                ));
            }
            if let Some((store, entry_index, range)) = operation.pending {
                return Ok(Step::Missing {
                    store,
                    entry_index,
                    range,
                });
            }
        } else {
            let (store, entry_index, _) = operation.pending.ok_or_else(|| {
                KernelError::new(
                    ErrorCode::InvalidInput,
                    "legacy step supplied a dictionary entry before one was requested",
                )
            })?;
            if supplied_store != store_code(store)
                || supplied_entry_index != entry_index
                || supplied_entry_index == NO_DICTIONARY
            {
                return Err(KernelError::new(
                    ErrorCode::InvalidInput,
                    "legacy step supplied the wrong dictionary entry",
                ));
            }
        }

        let mut analyzer = lock(&kernel.inner, "kernel")?;
        with_dictionary_stores(lexicon, locale, fallback, |stores| {
            if compressed_bytes != 0 {
                hydrate_store(
                    stores,
                    operation.pending.expect("validated pending dictionary").0,
                    supplied_entry_index,
                    input_bytes(compressed, compressed_bytes, "compressed dictionary block")?,
                )?;
                operation.pending = None;
            }
            let LegacyOperationState {
                analysis,
                session,
                method,
                pending,
                completed,
            } = &mut *operation;
            match analyzer.serialize_legacy_detailed_json(session, analysis, stores, *method)? {
                LegacyDetailStep::Ready(value) => {
                    *completed = true;
                    Ok(Step::Ready(value))
                }
                LegacyDetailStep::Missing {
                    store,
                    entry_index,
                    range,
                } => {
                    *pending = Some((store, entry_index, range));
                    Ok(Step::Missing {
                        store,
                        entry_index,
                        range,
                    })
                }
            }
        })
    })
}

#[unsafe(no_mangle)]
/// Begins one independently owned canonical token-details operation.
///
/// # Safety
///
/// Inputs are borrowed only for this call. `output` must be writable for one
/// operation pointer and the returned operation must be freed exactly once.
pub unsafe extern "C" fn ichiran_kernel_token_details_begin_utf16(
    kernel: *const IchiranKernel,
    input: *const u16,
    input_units: usize,
    options_json: *const u8,
    options_bytes: usize,
    path_index: usize,
    token_index: usize,
    output: *mut *mut IchiranTokenDetailsOperation,
) -> IchiranResult {
    boundary(|| {
        validate_pointer(output, 1, "token-details operation output")?;
        unsafe { *output = ptr::null_mut() };
        let kernel = unsafe { handle(kernel, "kernel")? };
        let input = input_units_slice(input, input_units)?;
        let options = parse_options(input_bytes(
            options_json,
            options_bytes,
            "analysis options",
        )?)?;
        let analysis = lock(&kernel.inner, "kernel")?.analyze_with_options(input, &options)?;
        if analysis
            .paths
            .get(path_index)
            .and_then(|path| path.tokens.get(token_index))
            .is_none()
        {
            return Err(KernelError::new(
                ErrorCode::OutOfRange,
                "analysis token was not found",
            ));
        }
        let operation = Box::new(IchiranTokenDetailsOperation {
            inner: Mutex::new(TokenDetailsOperationState {
                analysis,
                session: TokenDetailsSession::default(),
                path_index,
                token_index,
                pending: None,
                completed: false,
            }),
        });
        unsafe { *output = Box::into_raw(operation) };
        Ok(Vec::new())
    })
}

#[unsafe(no_mangle)]
/// Advances one canonical token-details operation by at most one dictionary block read.
///
/// # Safety
///
/// All handles must be live and must not be freed during this call. A supplied
/// compressed block is borrowed only for this call.
pub unsafe extern "C" fn ichiran_kernel_token_details_step(
    kernel: *const IchiranKernel,
    operation: *const IchiranTokenDetailsOperation,
    lexicon: *const IchiranLexiconStore,
    locale: *const IchiranLocaleStore,
    fallback: *const IchiranLocaleStore,
    supplied_store: u32,
    supplied_entry_index: u32,
    compressed: *const u8,
    compressed_bytes: usize,
) -> IchiranStepResult {
    step_boundary(|| {
        let kernel = unsafe { handle(kernel, "kernel")? };
        let operation = unsafe { handle(operation, "token-details operation")? };
        let lexicon = unsafe { handle(lexicon, "lexicon store")? };
        let locale = unsafe { handle(locale, "locale store")? };
        let fallback = unsafe { handle(fallback, "fallback locale store")? };
        let mut operation = lock(&operation.inner, "token-details operation")?;
        if operation.completed {
            return Err(KernelError::new(
                ErrorCode::InvalidInput,
                "token-details operation is already complete",
            ));
        }
        if compressed_bytes == 0 {
            if supplied_store != DICTIONARY_NONE
                || supplied_entry_index != NO_DICTIONARY
                || !compressed.is_null()
            {
                return Err(KernelError::new(
                    ErrorCode::InvalidInput,
                    "an empty token-details step must not supply a dictionary entry",
                ));
            }
            if let Some((store, entry_index, range)) = operation.pending {
                return Ok(Step::Missing {
                    store,
                    entry_index,
                    range,
                });
            }
        } else {
            let (store, entry_index, _) = operation.pending.ok_or_else(|| {
                KernelError::new(
                    ErrorCode::InvalidInput,
                    "token-details step supplied an entry before one was requested",
                )
            })?;
            if supplied_store != store_code(store)
                || supplied_entry_index != entry_index
                || supplied_entry_index == NO_DICTIONARY
            {
                return Err(KernelError::new(
                    ErrorCode::InvalidInput,
                    "token-details step supplied the wrong detail entry",
                ));
            }
        }

        let mut analyzer = lock(&kernel.inner, "kernel")?;
        with_dictionary_stores(lexicon, locale, fallback, |stores| {
            if compressed_bytes != 0 {
                hydrate_store(
                    stores,
                    operation.pending.expect("validated pending dictionary").0,
                    supplied_entry_index,
                    input_bytes(compressed, compressed_bytes, "compressed dictionary block")?,
                )?;
                operation.pending = None;
            }
            let TokenDetailsOperationState {
                analysis,
                session,
                path_index,
                token_index,
                pending,
                completed,
            } = &mut *operation;
            match analyzer.token_details_json(
                session,
                analysis,
                *path_index,
                *token_index,
                stores,
            )? {
                TokenDetailsStep::Ready(value) => {
                    *completed = true;
                    Ok(Step::Ready(value))
                }
                TokenDetailsStep::Missing {
                    store,
                    entry_index,
                    range,
                } => {
                    *pending = Some((store, entry_index, range));
                    Ok(Step::Missing {
                        store,
                        entry_index,
                        range,
                    })
                }
            }
        })
    })
}

fn with_dictionary_stores<T>(
    lexicon: &IchiranLexiconStore,
    locale: &IchiranLocaleStore,
    fallback: &IchiranLocaleStore,
    operation: impl FnOnce(&DictionaryStores<'_>) -> Result<T>,
) -> Result<T> {
    let same_locale = ptr::eq(locale, fallback);
    let lexicon = lock(&lexicon.inner, "lexicon store")?;
    let locale = lock(&locale.inner, "locale store")?;
    if same_locale {
        operation(&DictionaryStores {
            lexicon: &lexicon,
            locale: &locale,
            fallback: &locale,
        })
    } else {
        let fallback = lock(&fallback.inner, "fallback locale store")?;
        operation(&DictionaryStores {
            lexicon: &lexicon,
            locale: &locale,
            fallback: &fallback,
        })
    }
}

fn hydrate_store(
    stores: &DictionaryStores<'_>,
    store: DictionaryStoreKind,
    entry_index: u32,
    compressed: &[u8],
) -> Result<()> {
    match store {
        DictionaryStoreKind::Lexicon => stores
            .lexicon
            .entry_from_compressed(entry_index, compressed)
            .map(|_| ()),
        DictionaryStoreKind::Locale => stores
            .locale
            .entry_from_compressed(entry_index, compressed)
            .map(|_| ()),
        DictionaryStoreKind::Fallback => stores
            .fallback
            .entry_from_compressed(entry_index, compressed)
            .map(|_| ()),
    }
}

fn store_code(store: DictionaryStoreKind) -> u32 {
    match store {
        DictionaryStoreKind::Lexicon => 1,
        DictionaryStoreKind::Locale => 2,
        DictionaryStoreKind::Fallback => 3,
    }
}

#[unsafe(no_mangle)]
/// Releases one kernel handle.
///
/// # Safety
///
/// `kernel` must be null or a live handle returned by `ichiran_kernel_open`.
/// A non-null handle must be passed exactly once and not used concurrently.
pub unsafe extern "C" fn ichiran_kernel_free(kernel: *mut IchiranKernel) {
    if !kernel.is_null() {
        unsafe {
            drop(Box::from_raw(kernel));
        }
    }
}

#[unsafe(no_mangle)]
/// Releases one lexicon-store handle.
///
/// # Safety
///
/// `lexicon` must be null or a live handle returned by
/// `ichiran_lexicon_store_open`. A non-null handle must be passed exactly once
/// and not used concurrently.
pub unsafe extern "C" fn ichiran_lexicon_store_free(lexicon: *mut IchiranLexiconStore) {
    if !lexicon.is_null() {
        unsafe { drop(Box::from_raw(lexicon)) };
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn ichiran_locale_store_free(locale: *mut IchiranLocaleStore) {
    if !locale.is_null() {
        unsafe { drop(Box::from_raw(locale)) };
    }
}

#[unsafe(no_mangle)]
/// Releases one detailed/legacy operation handle.
///
/// # Safety
///
/// `operation` must be null or a live handle returned by
/// `ichiran_kernel_legacy_begin_utf16`. A non-null handle must be passed
/// exactly once and not used concurrently.
pub unsafe extern "C" fn ichiran_legacy_operation_free(operation: *mut IchiranLegacyOperation) {
    if !operation.is_null() {
        unsafe { drop(Box::from_raw(operation)) };
    }
}

#[unsafe(no_mangle)]
/// Releases one canonical token-details operation handle.
///
/// # Safety
///
/// `operation` must be null or a live handle returned by
/// `ichiran_kernel_token_details_begin_utf16`. A non-null handle must be passed
/// exactly once and not used concurrently.
pub unsafe extern "C" fn ichiran_token_details_operation_free(
    operation: *mut IchiranTokenDetailsOperation,
) {
    if !operation.is_null() {
        unsafe { drop(Box::from_raw(operation)) };
    }
}

#[unsafe(no_mangle)]
/// Releases one result buffer.
///
/// # Safety
///
/// `buffer` must be an unchanged buffer returned by this ABI and must be
/// passed exactly once.
pub unsafe extern "C" fn ichiran_buffer_free(buffer: IchiranBuffer) {
    if !buffer.data.is_null() {
        unsafe {
            drop(Vec::from_raw_parts(
                buffer.data,
                buffer.byte_length,
                buffer.capacity,
            ));
        }
    }
}

fn boundary(operation: impl FnOnce() -> Result<Vec<u8>>) -> IchiranResult {
    match catch_unwind(AssertUnwindSafe(operation)) {
        Ok(Ok(bytes)) => IchiranResult {
            status: 0,
            buffer: IchiranBuffer::from_vec(bytes),
        },
        Ok(Err(error)) => error_result(error),
        Err(_) => error_result(KernelError::new(
            ErrorCode::Internal,
            "Rust kernel panicked at the C boundary",
        )),
    }
}

enum Step {
    Ready(Vec<u8>),
    Missing {
        store: DictionaryStoreKind,
        entry_index: u32,
        range: DictionaryRange,
    },
}

fn step_boundary(operation: impl FnOnce() -> Result<Step>) -> IchiranStepResult {
    match catch_unwind(AssertUnwindSafe(operation)) {
        Ok(Ok(Step::Ready(bytes))) => IchiranStepResult {
            status: 0,
            state: 1,
            store: 0,
            entry_index: NO_DICTIONARY,
            range: IchiranDictionaryRange::default(),
            buffer: IchiranBuffer::from_vec(bytes),
        },
        Ok(Ok(Step::Missing {
            store,
            entry_index,
            range,
        })) => IchiranStepResult {
            status: 0,
            state: 2,
            store: store_code(store),
            entry_index,
            range: range.into(),
            buffer: IchiranBuffer::from_vec(Vec::new()),
        },
        Ok(Err(error)) => step_error(error),
        Err(_) => step_error(KernelError::new(
            ErrorCode::Internal,
            "Rust kernel panicked at the C boundary",
        )),
    }
}

fn step_error(error: KernelError) -> IchiranStepResult {
    let result = error_result(error);
    IchiranStepResult {
        status: result.status,
        state: 0,
        store: 0,
        entry_index: NO_DICTIONARY,
        range: IchiranDictionaryRange::default(),
        buffer: result.buffer,
    }
}

fn error_result(error: KernelError) -> IchiranResult {
    let bytes = serde_json::to_vec(&ErrorBody {
        code: error.code,
        message: &error.message,
    })
    .unwrap_or_else(|_| {
        b"{\"code\":\"internal\",\"message\":\"error serialization failed\"}".to_vec()
    });
    IchiranResult {
        status: status(error.code),
        buffer: IchiranBuffer::from_vec(bytes),
    }
}

fn parse_options(json: &[u8]) -> Result<AnalyzeOptions> {
    let options: CAnalyzeOptions = serde_json::from_slice(json).map_err(|error| {
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

fn parse_method(value: &[u8]) -> Result<Option<RomanizationName>> {
    let value = std::str::from_utf8(value).map_err(|_| {
        KernelError::new(
            ErrorCode::InvalidInput,
            "romanization method is not valid UTF-8",
        )
    })?;
    if value.is_empty() {
        return Ok(None);
    }
    RomanizationName::from_name(value).map(Some).ok_or_else(|| {
        KernelError::new(
            ErrorCode::InvalidInput,
            "romanization method is not supported",
        )
    })
}

fn serialize(value: &impl Serialize) -> Result<Vec<u8>> {
    serde_json::to_vec(value)
        .map_err(|error| KernelError::new(ErrorCode::Internal, error.to_string()))
}

unsafe fn handle<'a, T>(pointer: *const T, label: &str) -> Result<&'a T> {
    validate_pointer(pointer, 1, label)?;
    unsafe { pointer.as_ref() }.ok_or_else(|| {
        KernelError::new(ErrorCode::InvalidInput, format!("{label} pointer is null"))
    })
}

fn lock<'a, T>(mutex: &'a Mutex<T>, label: &str) -> Result<std::sync::MutexGuard<'a, T>> {
    mutex.lock().map_err(|_| {
        KernelError::new(
            ErrorCode::Internal,
            format!("{label} is unavailable after an earlier panic"),
        )
    })
}

fn status(code: ErrorCode) -> u32 {
    match code {
        ErrorCode::InvalidHeader => 1,
        ErrorCode::UnsupportedVersion => 2,
        ErrorCode::InvalidDirectory => 3,
        ErrorCode::CorruptSection => 4,
        ErrorCode::CorruptPayload => 5,
        ErrorCode::CorruptIndex => 6,
        ErrorCode::CorruptBlock => 7,
        ErrorCode::MissingSection => 8,
        ErrorCode::OutOfRange => 9,
        ErrorCode::InvalidInput => 10,
        ErrorCode::Internal => 11,
    }
}

fn input_bytes<'a>(pointer: *const u8, length: usize, label: &str) -> Result<&'a [u8]> {
    if length == 0 {
        return Ok(&[]);
    }
    validate_pointer(pointer, length, label)?;
    Ok(unsafe { slice::from_raw_parts(pointer, length) })
}

fn input_units_slice<'a>(pointer: *const u16, length: usize) -> Result<&'a [u16]> {
    if length == 0 {
        return Ok(&[]);
    }
    validate_pointer(pointer, length, "UTF-16 input")?;
    Ok(unsafe { slice::from_raw_parts(pointer, length) })
}

fn validate_pointer<T>(pointer: *const T, length: usize, label: &str) -> Result<()> {
    if length == 0 {
        return Ok(());
    }
    if pointer.is_null() {
        return Err(KernelError::new(
            ErrorCode::InvalidInput,
            format!("{label} pointer is null"),
        ));
    }
    let address = pointer as usize;
    if !address.is_multiple_of(align_of::<T>()) {
        return Err(KernelError::new(
            ErrorCode::InvalidInput,
            format!("{label} pointer is not aligned"),
        ));
    }
    let byte_length = length.checked_mul(size_of::<T>()).ok_or_else(|| {
        KernelError::new(
            ErrorCode::InvalidInput,
            format!("{label} byte length overflows"),
        )
    })?;
    if byte_length > isize::MAX as usize || address.checked_add(byte_length).is_none() {
        return Err(KernelError::new(
            ErrorCode::InvalidInput,
            format!("{label} range is too large"),
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::ptr::NonNull;

    use super::*;

    #[test]
    fn rejects_input_range_larger_than_rust_slices_allow() {
        let error = validate_pointer(
            NonNull::<u8>::dangling().as_ptr(),
            isize::MAX as usize + 1,
            "test input",
        )
        .unwrap_err();

        assert_eq!(error.code, ErrorCode::InvalidInput);
        assert_eq!(error.message, "test input range is too large");
    }

    #[test]
    fn rejects_misaligned_utf16_pointer_before_slice_creation() {
        let pointer = std::ptr::without_provenance::<u16>(1);
        let error = validate_pointer(pointer, 1, "UTF-16 input").unwrap_err();

        assert_eq!(error.code, ErrorCode::InvalidInput);
        assert_eq!(error.message, "UTF-16 input pointer is not aligned");
    }

    #[test]
    fn converts_panics_to_owned_internal_error_results() {
        let result = boundary(|| -> Result<Vec<u8>> { panic!("C boundary fixture") });

        assert_eq!(result.status, status(ErrorCode::Internal));
        let bytes = unsafe {
            slice::from_raw_parts(result.buffer.data, result.buffer.byte_length).to_vec()
        };
        assert_eq!(
            bytes,
            br#"{"code":"internal","message":"Rust kernel panicked at the C boundary"}"#
        );
        unsafe { ichiran_buffer_free(result.buffer) };
    }

    #[test]
    fn converts_step_panics_to_owned_internal_error_results() {
        let result = step_boundary(|| -> Result<Step> { panic!("C step boundary fixture") });

        assert_eq!(result.status, status(ErrorCode::Internal));
        assert_eq!(result.state, 0);
        let bytes = unsafe {
            slice::from_raw_parts(result.buffer.data, result.buffer.byte_length).to_vec()
        };
        assert_eq!(
            bytes,
            br#"{"code":"internal","message":"Rust kernel panicked at the C boundary"}"#
        );
        unsafe { ichiran_buffer_free(result.buffer) };
    }

    #[test]
    fn parses_the_versioned_c_options_document() {
        let options = parse_options(
            br#"{"limit":3,"entities":[{"start":1,"end":2,"boost":4.5}],"normalizePunctuation":true}"#,
        )
        .unwrap();

        assert_eq!(options.limit, 3);
        assert_eq!(options.entities.len(), 1);
        assert_eq!(options.entities[0].start, 1);
        assert_eq!(options.entities[0].end, 2);
        assert_eq!(options.entities[0].boost, Some(4.5));
        assert!(options.normalize_punctuation);
    }
}
