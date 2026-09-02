#ifndef ICHIRAN_KERNEL_H
#define ICHIRAN_KERNEL_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ICHIRAN_KERNEL_ABI_VERSION 2u

typedef struct IchiranKernel IchiranKernel;

typedef enum IchiranStatus {
  ICHIRAN_OK = 0,
  ICHIRAN_INVALID_HEADER = 1,
  ICHIRAN_UNSUPPORTED_VERSION = 2,
  ICHIRAN_INVALID_DIRECTORY = 3,
  ICHIRAN_CORRUPT_SECTION = 4,
  ICHIRAN_CORRUPT_PAYLOAD = 5,
  ICHIRAN_CORRUPT_INDEX = 6,
  ICHIRAN_CORRUPT_BLOCK = 7,
  ICHIRAN_MISSING_SECTION = 8,
  ICHIRAN_OUT_OF_RANGE = 9,
  ICHIRAN_INVALID_INPUT = 10,
  ICHIRAN_INTERNAL = 11
} IchiranStatus;

/*
 * Rust owns this allocation. Pass the complete, unchanged value exactly once
 * to ichiran_buffer_free, including on errors. Do not free data directly.
 */
typedef struct IchiranBuffer {
  uint8_t *data;
  size_t byte_length;
  size_t capacity;
} IchiranBuffer;

/* status == ICHIRAN_OK: buffer is UTF-8 JSON or empty. Otherwise it is a JSON error. */
typedef struct IchiranResult {
  uint32_t status;
  IchiranBuffer buffer;
} IchiranResult;

uint32_t ichiran_kernel_abi_version(void);

/*
 * Copies and strictly validates hot[0..hot_bytes]. On success, *output is owned
 * by the caller and must be released once with ichiran_kernel_free. output must
 * be non-NULL, aligned, and writable. hot may be NULL only when hot_bytes is
 * zero; otherwise it must remain readable until this call returns.
 */
IchiranResult ichiran_kernel_open(
  const uint8_t *hot,
  size_t hot_bytes,
  IchiranKernel **output
);

/*
 * Executes one coarse operation. Input lengths and all returned spans are
 * UTF-16 code units. The input may contain unpaired surrogates. Concurrent
 * analysis calls on one kernel are safe and execute serially. The caller must
 * not release the kernel until every analysis call has returned. kernel must be
 * a live handle returned by ichiran_kernel_open. input may be NULL only when
 * input_units is zero; otherwise it must be aligned and remain readable until
 * this call returns. options_json is one UTF-8 JSON object with exactly
 * `limit`, `entities`, and `normalizePunctuation`; it is borrowed only for this
 * call and may be NULL only when options_bytes is zero. Each entity has integer
 * `start` and `end` fields and an optional finite numeric `boost` (omitted or
 * null means no explicit boost). Entity offsets and all result spans are
 * UTF-16 code units. Invalid JSON returns
 * ICHIRAN_INVALID_INPUT with an owned JSON error buffer.
 */
IchiranResult ichiran_kernel_analyze_utf16(
  const IchiranKernel *kernel,
  const uint16_t *input,
  size_t input_units,
  const uint8_t *options_json,
  size_t options_bytes
);

/* Accepts NULL; every non-NULL kernel handle must be released exactly once. */
void ichiran_kernel_free(IchiranKernel *kernel);

/* Accepts only an unchanged result buffer, exactly once. */
void ichiran_buffer_free(IchiranBuffer buffer);

#ifdef __cplusplus
}
#endif

#endif
