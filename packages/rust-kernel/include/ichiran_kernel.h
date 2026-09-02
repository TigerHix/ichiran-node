#ifndef ICHIRAN_KERNEL_H
#define ICHIRAN_KERNEL_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ICHIRAN_KERNEL_ABI_VERSION 3u
#define ICHIRAN_NO_DETAIL UINT32_MAX

typedef struct IchiranKernel IchiranKernel;
typedef struct IchiranDetailStore IchiranDetailStore;
typedef struct IchiranLegacyOperation IchiranLegacyOperation;

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

typedef enum IchiranStepState {
  ICHIRAN_STEP_ERROR = 0,
  ICHIRAN_STEP_READY = 1,
  ICHIRAN_STEP_MISSING_DETAIL = 2
} IchiranStepState;

/* Rust owns this allocation. Return the complete value exactly once. */
typedef struct IchiranBuffer {
  uint8_t *data;
  size_t byte_length;
  size_t capacity;
} IchiranBuffer;

/* OK carries UTF-8 JSON or empty; errors carry an owned JSON error. */
typedef struct IchiranResult {
  uint32_t status;
  IchiranBuffer buffer;
} IchiranResult;

typedef struct IchiranDetailRange {
  uint32_t block;
  uint32_t offset;
  uint32_t byte_length;
  uint32_t uncompressed_bytes;
  uint32_t checksum;
} IchiranDetailRange;

/*
 * READY carries exact detailed legacy JSON. MISSING_DETAIL carries entry_index
 * and range with an empty buffer. ERROR carries an owned JSON error buffer.
 */
typedef struct IchiranStepResult {
  uint32_t status;
  uint32_t state;
  uint32_t entry_index;
  IchiranDetailRange range;
  IchiranBuffer buffer;
} IchiranStepResult;

uint32_t ichiran_kernel_abi_version(void);

/* Copies and strictly validates the complete hot pack. */
IchiranResult ichiran_kernel_open(
  const uint8_t *hot,
  size_t hot_bytes,
  IchiranKernel **output
);

/*
 * Executes one clean analysis. Input lengths, entity offsets, and result spans
 * are UTF-16 code units, including unpaired surrogates. options_json is exactly
 * {limit, entities, normalizePunctuation}.
 */
IchiranResult ichiran_kernel_analyze_utf16(
  const IchiranKernel *kernel,
  const uint16_t *input,
  size_t input_units,
  const uint8_t *options_json,
  size_t options_bytes
);

/*
 * Executes analyzer-backed romanization. An empty method selects traditional
 * Hepburn. The result is a lossless JSON string whose escapes preserve UTF-16.
 */
IchiranResult ichiran_kernel_romanize_utf16(
  const IchiranKernel *kernel,
  const uint16_t *input,
  size_t input_units,
  const uint8_t *options_json,
  size_t options_bytes,
  const uint8_t *method_utf8,
  size_t method_bytes
);

/* Reads the resident prefix length from the complete 96-byte detail header. */
IchiranResult ichiran_detail_prefix_length(
  const uint8_t *header,
  size_t header_bytes,
  size_t total_bytes,
  size_t *output
);

/* Copies and validates the resident prefix; details.bin remains host-owned. */
IchiranResult ichiran_detail_store_open(
  const uint8_t *prefix,
  size_t prefix_bytes,
  size_t total_bytes,
  IchiranDetailStore **output
);

IchiranResult ichiran_detail_store_range(
  const IchiranDetailStore *details,
  uint32_t entry_index,
  IchiranDetailRange *output
);

/* Validates one exact compressed range and returns decoded DetailEntry JSON. */
IchiranResult ichiran_detail_store_decode(
  const IchiranDetailStore *details,
  uint32_t entry_index,
  const uint8_t *compressed,
  size_t compressed_bytes
);

/* Analyzes once and creates one independently owned detailed/legacy session. */
IchiranResult ichiran_kernel_legacy_begin_utf16(
  const IchiranKernel *kernel,
  const uint16_t *input,
  size_t input_units,
  const uint8_t *options_json,
  size_t options_bytes,
  const uint8_t *method_utf8,
  size_t method_bytes,
  IchiranLegacyOperation **output
);

/*
 * First call: ICHIRAN_NO_DETAIL, NULL, zero. On MISSING_DETAIL, read exactly
 * result.range and pass its bytes with result.entry_index on the next call.
 * Rust atomically decodes and retries. READY is terminal.
 */
IchiranStepResult ichiran_kernel_legacy_step(
  const IchiranKernel *kernel,
  const IchiranLegacyOperation *operation,
  const IchiranDetailStore *details,
  uint32_t supplied_entry_index,
  const uint8_t *compressed,
  size_t compressed_bytes
);

/*
 * A kernel and detail store may be shared by native threads. Independent
 * legacy operation handles retain independent sessions. Do not free a handle
 * while a call uses it. All input pointers are borrowed only for the call. All
 * fallible entries contain Rust panics and report ICHIRAN_INTERNAL.
 */
void ichiran_kernel_free(IchiranKernel *kernel);
void ichiran_detail_store_free(IchiranDetailStore *details);
void ichiran_legacy_operation_free(IchiranLegacyOperation *operation);

/* Return every unchanged result or step buffer exactly once, including errors. */
void ichiran_buffer_free(IchiranBuffer buffer);

#ifdef __cplusplus
}
#endif

#endif
