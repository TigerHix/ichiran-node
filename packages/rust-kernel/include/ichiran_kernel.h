#ifndef ICHIRAN_KERNEL_H
#define ICHIRAN_KERNEL_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

#define ICHIRAN_KERNEL_ABI_VERSION 7u
#define ICHIRAN_NO_DICTIONARY UINT32_MAX

typedef struct IchiranKernel IchiranKernel;
typedef struct IchiranLexiconStore IchiranLexiconStore;
typedef struct IchiranLocaleStore IchiranLocaleStore;
typedef struct IchiranLegacyOperation IchiranLegacyOperation;
typedef struct IchiranTokenDetailsOperation IchiranTokenDetailsOperation;

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
  ICHIRAN_STEP_MISSING_DICTIONARY = 2
} IchiranStepState;

typedef enum IchiranDictionaryStoreKind {
  ICHIRAN_DICTIONARY_NONE = 0,
  ICHIRAN_DICTIONARY_LEXICON = 1,
  ICHIRAN_DICTIONARY_LOCALE = 2,
  ICHIRAN_DICTIONARY_FALLBACK = 3
} IchiranDictionaryStoreKind;

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

typedef struct IchiranDictionaryRange {
  uint32_t block;
  uint32_t offset;
  uint32_t byte_length;
  uint32_t uncompressed_bytes;
  uint32_t checksum;
} IchiranDictionaryRange;

/*
 * READY carries exact detailed legacy JSON. MISSING_DICTIONARY carries a store,
 * entry_index, and range with an empty buffer. ERROR carries owned JSON.
 */
typedef struct IchiranStepResult {
  uint32_t status;
  uint32_t state;
  uint32_t store;
  uint32_t entry_index;
  IchiranDictionaryRange range;
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
 * {limit, entities, normalizePunctuation}. Selected candidates are absent from
 * alternatives; reading hints and legacy counter labels are not exposed.
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

/* Reads resident prefix lengths from complete fixed headers. */
IchiranResult ichiran_lexicon_prefix_length(
  const uint8_t *header,
  size_t header_bytes,
  size_t total_bytes,
  size_t *output
);

IchiranResult ichiran_locale_prefix_length(
  const uint8_t *header,
  size_t header_bytes,
  size_t total_bytes,
  size_t *output
);

IchiranResult ichiran_lexicon_store_open(
  const uint8_t *prefix,
  size_t prefix_bytes,
  size_t total_bytes,
  IchiranLexiconStore **output
);

size_t ichiran_lexicon_store_entry_count(const IchiranLexiconStore *lexicon);

IchiranResult ichiran_lexicon_store_range(
  const IchiranLexiconStore *lexicon,
  uint32_t entry_index,
  IchiranDictionaryRange *output
);

IchiranResult ichiran_lexicon_store_decode(
  const IchiranLexiconStore *lexicon,
  uint32_t entry_index,
  const uint8_t *compressed,
  size_t compressed_bytes
);

IchiranResult ichiran_locale_store_open(
  const uint8_t *prefix,
  size_t prefix_bytes,
  size_t total_bytes,
  const uint8_t lexicon_sha256[32],
  const uint8_t *locale_utf8,
  size_t locale_bytes,
  size_t lexicon_entry_count,
  IchiranLocaleStore **output
);

IchiranResult ichiran_locale_store_range(
  const IchiranLocaleStore *locale,
  uint32_t entry_index,
  IchiranDictionaryRange *output
);

IchiranResult ichiran_locale_store_decode(
  const IchiranLocaleStore *locale,
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
 * First call: ICHIRAN_DICTIONARY_NONE, ICHIRAN_NO_DICTIONARY, NULL, zero. On
 * MISSING_DICTIONARY, read from result.store and pass the exact result.range
 * bytes with result.store and result.entry_index on the next call.
 * Rust atomically decodes and retries. READY is terminal.
 */
IchiranStepResult ichiran_kernel_legacy_step(
  const IchiranKernel *kernel,
  const IchiranLegacyOperation *operation,
  const IchiranLexiconStore *lexicon,
  const IchiranLocaleStore *locale,
  const IchiranLocaleStore *fallback,
  uint32_t supplied_store,
  uint32_t supplied_entry_index,
  const uint8_t *compressed,
  size_t compressed_bytes
);

/* Analyzes once and selects one token from one ranked global path. */
IchiranResult ichiran_kernel_token_details_begin_utf16(
  const IchiranKernel *kernel,
  const uint16_t *input,
  size_t input_units,
  const uint8_t *options_json,
  size_t options_bytes,
  size_t path_index,
  size_t token_index,
  IchiranTokenDetailsOperation **output
);

/*
 * Same lazy range handshake as ichiran_kernel_legacy_step. READY carries canonical
 * TokenDetails JSON with clean reading, alternatives, semantic suffixId/entityKind,
 * and structured counter semantics. Presentation strings belong to the host.
 */
IchiranStepResult ichiran_kernel_token_details_step(
  const IchiranKernel *kernel,
  const IchiranTokenDetailsOperation *operation,
  const IchiranLexiconStore *lexicon,
  const IchiranLocaleStore *locale,
  const IchiranLocaleStore *fallback,
  uint32_t supplied_store,
  uint32_t supplied_entry_index,
  const uint8_t *compressed,
  size_t compressed_bytes
);

/*
 * A kernel and dictionary stores may be shared by native threads. Independent
 * operation handles retain independent sessions. Do not free a handle while a
 * call uses it. All input pointers are borrowed only for the call. All fallible
 * entries contain Rust panics and report ICHIRAN_INTERNAL.
 */
void ichiran_kernel_free(IchiranKernel *kernel);
void ichiran_lexicon_store_free(IchiranLexiconStore *lexicon);
void ichiran_locale_store_free(IchiranLocaleStore *locale);
void ichiran_legacy_operation_free(IchiranLegacyOperation *operation);
void ichiran_token_details_operation_free(IchiranTokenDetailsOperation *operation);

/* Return every unchanged result or step buffer exactly once, including errors. */
void ichiran_buffer_free(IchiranBuffer buffer);

#ifdef __cplusplus
}
#endif

#endif
