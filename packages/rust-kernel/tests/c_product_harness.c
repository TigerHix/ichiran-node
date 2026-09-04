#define _POSIX_C_SOURCE 200809L

#include "ichiran_kernel.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEXICON_HEADER_BYTES 96u
#define LOCALE_HEADER_BYTES 128u

typedef struct DictionaryFiles {
  FILE *lexicon;
  FILE *english;
  FILE *chinese;
} DictionaryFiles;

static int file_length(FILE *file, size_t *output) {
  if (fseek(file, 0, SEEK_END) != 0) return 0;
  const long length = ftell(file);
  if (length <= 0 || fseek(file, 0, SEEK_SET) != 0) return 0;
  *output = (size_t)length;
  return 1;
}

static uint8_t *read_file(const char *path, size_t *byte_length) {
  FILE *file = fopen(path, "rb");
  if (file == NULL || !file_length(file, byte_length)) {
    if (file != NULL) fclose(file);
    return NULL;
  }
  uint8_t *bytes = malloc(*byte_length);
  if (bytes == NULL || fread(bytes, 1, *byte_length, file) != *byte_length) {
    free(bytes);
    fclose(file);
    return NULL;
  }
  fclose(file);
  return bytes;
}

static int read_range(FILE *file, const IchiranDictionaryRange *range, uint8_t **output) {
  uint8_t *bytes = malloc(range->byte_length);
  if (bytes == NULL || fseek(file, (long)range->offset, SEEK_SET) != 0
      || fread(bytes, 1, range->byte_length, file) != range->byte_length) {
    free(bytes);
    return 0;
  }
  *output = bytes;
  return 1;
}

static int open_kernel(const char *path, IchiranKernel **output) {
  size_t bytes = 0;
  uint8_t *input = read_file(path, &bytes);
  if (input == NULL) return 0;
  IchiranResult result = ichiran_kernel_open(input, bytes, output);
  free(input);
  const int passed = result.status == ICHIRAN_OK && *output != NULL;
  ichiran_buffer_free(result.buffer);
  return passed;
}

static int read_prefix(
  FILE *file,
  size_t header_bytes,
  IchiranResult (*prefix_length)(const uint8_t *, size_t, size_t, size_t *),
  uint8_t **output,
  size_t *prefix_bytes,
  size_t *total_bytes
) {
  uint8_t header[LOCALE_HEADER_BYTES];
  if (header_bytes > sizeof(header) || !file_length(file, total_bytes)
      || fread(header, 1, header_bytes, file) != header_bytes) return 0;
  IchiranResult length = prefix_length(header, header_bytes, *total_bytes, prefix_bytes);
  const int valid = length.status == ICHIRAN_OK && *prefix_bytes >= header_bytes;
  ichiran_buffer_free(length.buffer);
  if (!valid) return 0;
  uint8_t *prefix = malloc(*prefix_bytes);
  if (prefix == NULL || fseek(file, 0, SEEK_SET) != 0
      || fread(prefix, 1, *prefix_bytes, file) != *prefix_bytes) {
    free(prefix);
    return 0;
  }
  *output = prefix;
  return 1;
}

static int open_lexicon(const char *path, FILE **file_output, IchiranLexiconStore **store_output) {
  FILE *file = fopen(path, "rb");
  uint8_t *prefix = NULL;
  size_t prefix_bytes = 0;
  size_t total_bytes = 0;
  if (file == NULL || !read_prefix(
      file, LEXICON_HEADER_BYTES, ichiran_lexicon_prefix_length,
      &prefix, &prefix_bytes, &total_bytes
  )) {
    if (file != NULL) fclose(file);
    return 0;
  }
  IchiranResult result = ichiran_lexicon_store_open(
    prefix, prefix_bytes, total_bytes, store_output
  );
  free(prefix);
  const int passed = result.status == ICHIRAN_OK && *store_output != NULL;
  ichiran_buffer_free(result.buffer);
  if (!passed) {
    fclose(file);
    return 0;
  }
  *file_output = file;
  return 1;
}

static int open_locale(
  const char *path,
  const uint8_t digest[32],
  const char *locale,
  size_t entry_count,
  FILE **file_output,
  IchiranLocaleStore **store_output
) {
  FILE *file = fopen(path, "rb");
  uint8_t *prefix = NULL;
  size_t prefix_bytes = 0;
  size_t total_bytes = 0;
  if (file == NULL || !read_prefix(
      file, LOCALE_HEADER_BYTES, ichiran_locale_prefix_length,
      &prefix, &prefix_bytes, &total_bytes
  )) {
    if (file != NULL) fclose(file);
    return 0;
  }
  IchiranResult result = ichiran_locale_store_open(
    prefix, prefix_bytes, total_bytes, digest, (const uint8_t *)locale,
    strlen(locale), entry_count, store_output
  );
  free(prefix);
  const int passed = result.status == ICHIRAN_OK && *store_output != NULL;
  ichiran_buffer_free(result.buffer);
  if (!passed) {
    fclose(file);
    return 0;
  }
  *file_output = file;
  return 1;
}

static int locale_digest(const char *path, uint8_t output[32]) {
  FILE *file = fopen(path, "rb");
  uint8_t header[LOCALE_HEADER_BYTES];
  if (file == NULL || fread(header, 1, sizeof(header), file) != sizeof(header)) {
    if (file != NULL) fclose(file);
    return 0;
  }
  fclose(file);
  memcpy(output, header + 60, 32);
  return 1;
}

static FILE *file_for_store(DictionaryFiles *files, FILE *locale_file, uint32_t store) {
  if (store == ICHIRAN_DICTIONARY_LEXICON) return files->lexicon;
  if (store == ICHIRAN_DICTIONARY_LOCALE) return locale_file;
  if (store == ICHIRAN_DICTIONARY_FALLBACK) return files->english;
  return NULL;
}

static int run_legacy(
  const IchiranKernel *kernel,
  const IchiranLexiconStore *lexicon,
  const IchiranLocaleStore *locale,
  const IchiranLocaleStore *fallback,
  FILE *locale_file,
  DictionaryFiles *files
) {
  static const uint16_t input[] = {0x732b};
  static const uint8_t options[] =
    "{\"limit\":1,\"entities\":[],\"normalizePunctuation\":true}";
  IchiranLegacyOperation *operation = NULL;
  IchiranResult begun = ichiran_kernel_legacy_begin_utf16(
    kernel, input, 1, options, sizeof(options) - 1, NULL, 0, &operation
  );
  int passed = begun.status == ICHIRAN_OK && operation != NULL;
  ichiran_buffer_free(begun.buffer);
  uint32_t supplied_store = ICHIRAN_DICTIONARY_NONE;
  uint32_t supplied_entry = ICHIRAN_NO_DICTIONARY;
  uint8_t *compressed = NULL;
  size_t compressed_bytes = 0;
  for (size_t step_index = 0; passed && step_index < 128; step_index++) {
    IchiranStepResult step = ichiran_kernel_legacy_step(
      kernel, operation, lexicon, locale, fallback, supplied_store, supplied_entry,
      compressed, compressed_bytes
    );
    free(compressed);
    compressed = NULL;
    compressed_bytes = 0;
    supplied_store = ICHIRAN_DICTIONARY_NONE;
    supplied_entry = ICHIRAN_NO_DICTIONARY;
    if (step.status != ICHIRAN_OK) passed = 0;
    else if (step.state == ICHIRAN_STEP_READY) {
      passed = step.buffer.byte_length > 0;
      ichiran_buffer_free(step.buffer);
      break;
    } else if (step.state == ICHIRAN_STEP_MISSING_DICTIONARY) {
      FILE *file = file_for_store(files, locale_file, step.store);
      passed = file != NULL && read_range(file, &step.range, &compressed);
      supplied_store = step.store;
      supplied_entry = step.entry_index;
      compressed_bytes = step.range.byte_length;
    } else passed = 0;
    ichiran_buffer_free(step.buffer);
  }
  free(compressed);
  ichiran_legacy_operation_free(operation);
  return passed;
}

static int run_token_details(
  const IchiranKernel *kernel,
  const IchiranLexiconStore *lexicon,
  const IchiranLocaleStore *locale,
  const IchiranLocaleStore *fallback,
  FILE *locale_file,
  DictionaryFiles *files
) {
  static const uint16_t input[] = {0x732b};
  static const uint8_t options[] =
    "{\"limit\":1,\"entities\":[],\"normalizePunctuation\":true}";
  IchiranTokenDetailsOperation *operation = NULL;
  IchiranResult begun = ichiran_kernel_token_details_begin_utf16(
    kernel, input, 1, options, sizeof(options) - 1, 0, 0, &operation
  );
  int passed = begun.status == ICHIRAN_OK && operation != NULL;
  ichiran_buffer_free(begun.buffer);
  uint32_t supplied_store = ICHIRAN_DICTIONARY_NONE;
  uint32_t supplied_entry = ICHIRAN_NO_DICTIONARY;
  uint8_t *compressed = NULL;
  size_t compressed_bytes = 0;
  for (size_t step_index = 0; passed && step_index < 128; step_index++) {
    IchiranStepResult step = ichiran_kernel_token_details_step(
      kernel, operation, lexicon, locale, fallback, supplied_store, supplied_entry,
      compressed, compressed_bytes
    );
    free(compressed);
    compressed = NULL;
    compressed_bytes = 0;
    supplied_store = ICHIRAN_DICTIONARY_NONE;
    supplied_entry = ICHIRAN_NO_DICTIONARY;
    if (step.status != ICHIRAN_OK) passed = 0;
    else if (step.state == ICHIRAN_STEP_READY) {
      passed = step.buffer.byte_length > 0;
      ichiran_buffer_free(step.buffer);
      break;
    } else if (step.state == ICHIRAN_STEP_MISSING_DICTIONARY) {
      FILE *file = file_for_store(files, locale_file, step.store);
      passed = file != NULL && read_range(file, &step.range, &compressed);
      supplied_store = step.store;
      supplied_entry = step.entry_index;
      compressed_bytes = step.range.byte_length;
    } else passed = 0;
    ichiran_buffer_free(step.buffer);
  }
  free(compressed);
  ichiran_token_details_operation_free(operation);
  return passed;
}

static int decode_entry_zero(
  FILE *file,
  const IchiranLexiconStore *lexicon,
  const IchiranLocaleStore *locale
) {
  IchiranDictionaryRange range;
  IchiranResult ranged = lexicon != NULL
    ? ichiran_lexicon_store_range(lexicon, 0, &range)
    : ichiran_locale_store_range(locale, 0, &range);
  int passed = ranged.status == ICHIRAN_OK && range.byte_length > 0;
  ichiran_buffer_free(ranged.buffer);
  uint8_t *compressed = NULL;
  if (!passed || !read_range(file, &range, &compressed)) return 0;
  IchiranResult decoded = lexicon != NULL
    ? ichiran_lexicon_store_decode(lexicon, 0, compressed, range.byte_length)
    : ichiran_locale_store_decode(locale, 0, compressed, range.byte_length);
  free(compressed);
  passed = decoded.status == ICHIRAN_OK && decoded.buffer.byte_length > 0;
  ichiran_buffer_free(decoded.buffer);
  return passed;
}

int main(int argc, char **argv) {
  if (argc != 5) {
    fputs("usage: c_product_harness <hot.bin> <lexicon.bin> <gloss.en.bin> <gloss.zh-Hans.bin>\n", stderr);
    return 2;
  }
  if (ichiran_kernel_abi_version() != ICHIRAN_KERNEL_ABI_VERSION) return 3;
  uint8_t digest[32];
  IchiranKernel *kernel = NULL;
  IchiranLexiconStore *lexicon = NULL;
  IchiranLocaleStore *english = NULL;
  IchiranLocaleStore *chinese = NULL;
  DictionaryFiles files = {0};
  int passed = locale_digest(argv[3], digest)
    && open_kernel(argv[1], &kernel)
    && open_lexicon(argv[2], &files.lexicon, &lexicon)
    && open_locale(
      argv[3], digest, "en", ichiran_lexicon_store_entry_count(lexicon),
      &files.english, &english
    )
    && open_locale(
      argv[4], digest, "zh-Hans", ichiran_lexicon_store_entry_count(lexicon),
      &files.chinese, &chinese
    );
  if (passed) {
    passed = decode_entry_zero(files.lexicon, lexicon, NULL)
      && decode_entry_zero(files.english, NULL, english)
      && decode_entry_zero(files.chinese, NULL, chinese)
      && run_legacy(kernel, lexicon, english, english, files.english, &files)
      && run_legacy(kernel, lexicon, chinese, english, files.chinese, &files)
      && run_token_details(kernel, lexicon, chinese, english, files.chinese, &files);
  }
  if (files.lexicon != NULL) fclose(files.lexicon);
  if (files.english != NULL) fclose(files.english);
  if (files.chinese != NULL) fclose(files.chinese);
  ichiran_locale_store_free(chinese);
  ichiran_locale_store_free(english);
  ichiran_lexicon_store_free(lexicon);
  ichiran_kernel_free(kernel);
  if (!passed) {
    fputs("C ABI v7 multilingual product harness failed\n", stderr);
    return 4;
  }
  puts("C ABI v7 multilingual product harness passed: lexicon/en/zh-Hans lazy decode and localized legacy/token handshakes");
  return 0;
}
