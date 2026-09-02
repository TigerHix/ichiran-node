use std::mem::{align_of, size_of};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::ptr;
use std::slice;
use std::sync::Mutex;

use serde::{Deserialize, Serialize};

use crate::{AnalyzeOptions, EntityHint, ErrorCode, Kernel, KernelError, Result};

const ABI_VERSION: u32 = 2;

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

pub struct IchiranKernel {
    inner: Mutex<Kernel>,
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
        validate_pointer(kernel, 1, "kernel")?;
        let kernel = unsafe { kernel.as_ref() }
            .ok_or_else(|| KernelError::new(ErrorCode::InvalidInput, "kernel pointer is null"))?;
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
