#define _POSIX_C_SOURCE 200809L

#include "ichiran_kernel.h"

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DETAIL_HEADER_BYTES 96u
#define DETAILED_CASES 705u
#define ROMANIZATION_CASES 8u
#define DESCRIBE_CASES 4u
#define THREAD_COUNT 4u
#define CONCURRENT_REPEATS 4u

typedef struct LegacyCase {
  char *name;
  uint16_t *input;
  size_t input_units;
  uint8_t *options;
  size_t options_bytes;
  uint8_t *expected;
  size_t expected_bytes;
} LegacyCase;

typedef struct LegacyThread {
  const IchiranKernel *kernel;
  const IchiranDetailStore *details;
  const char *details_path;
  const LegacyCase *first;
  const LegacyCase *second;
  int passed;
} LegacyThread;

static void *copy_bytes(const void *value, size_t byte_length) {
  if (byte_length == 0) return NULL;
  void *copy = malloc(byte_length);
  if (copy != NULL) memcpy(copy, value, byte_length);
  return copy;
}

static uint8_t *read_file(const char *path, size_t *byte_length) {
  FILE *file = fopen(path, "rb");
  if (file == NULL || fseek(file, 0, SEEK_END) != 0) {
    if (file != NULL) fclose(file);
    return NULL;
  }
  const long length = ftell(file);
  if (length <= 0 || fseek(file, 0, SEEK_SET) != 0) {
    fclose(file);
    return NULL;
  }
  uint8_t *bytes = malloc((size_t)length);
  if (bytes == NULL || fread(bytes, 1, (size_t)length, file) != (size_t)length) {
    free(bytes);
    fclose(file);
    return NULL;
  }
  fclose(file);
  *byte_length = (size_t)length;
  return bytes;
}

static int file_length(FILE *file, size_t *output) {
  if (fseek(file, 0, SEEK_END) != 0) return 0;
  const long length = ftell(file);
  if (length <= 0 || fseek(file, 0, SEEK_SET) != 0) return 0;
  *output = (size_t)length;
  return 1;
}

static int read_range(FILE *file, uint32_t offset, uint32_t byte_length, uint8_t **output) {
  uint8_t *bytes = malloc(byte_length);
  if (bytes == NULL || fseek(file, (long)offset, SEEK_SET) != 0
      || fread(bytes, 1, byte_length, file) != byte_length) {
    free(bytes);
    return 0;
  }
  *output = bytes;
  return 1;
}

static void print_buffer(const IchiranBuffer *buffer) {
  const size_t shown = buffer->byte_length < 600 ? buffer->byte_length : 600;
  if (shown > 0) fwrite(buffer->data, 1, shown, stderr);
  if (shown < buffer->byte_length) fputs("...", stderr);
}

static int exact_buffer(const IchiranBuffer *actual, const uint8_t *expected, size_t bytes) {
  return actual->byte_length == bytes
    && (bytes == 0 || memcmp(actual->data, expected, bytes) == 0);
}

static int parse_utf16(char *hex, uint16_t **output, size_t *output_units) {
  if (*hex == '\0') {
    *output = NULL;
    *output_units = 0;
    return 1;
  }
  size_t units = 1;
  for (const char *cursor = hex; *cursor != '\0'; cursor++) {
    if (*cursor == ',') units++;
  }
  uint16_t *result = malloc(units * sizeof(*result));
  if (result == NULL) return 0;
  char *cursor = hex;
  for (size_t index = 0; index < units; index++) {
    errno = 0;
    char *end = NULL;
    const unsigned long value = strtoul(cursor, &end, 16);
    if (errno != 0 || end == cursor || value > UINT16_MAX
        || (*end != ',' && *end != '\0')) {
      free(result);
      return 0;
    }
    result[index] = (uint16_t)value;
    cursor = *end == ',' ? end + 1 : end;
  }
  *output = result;
  *output_units = units;
  return 1;
}

static void free_legacy_case(LegacyCase *value) {
  free(value->name);
  free(value->input);
  free(value->options);
  free(value->expected);
  memset(value, 0, sizeof(*value));
}

static int copy_legacy_case(const LegacyCase *source, LegacyCase *output) {
  memset(output, 0, sizeof(*output));
  output->name = copy_bytes(source->name, strlen(source->name) + 1);
  output->input = copy_bytes(source->input, source->input_units * sizeof(*source->input));
  output->input_units = source->input_units;
  output->options = copy_bytes(source->options, source->options_bytes);
  output->options_bytes = source->options_bytes;
  output->expected = copy_bytes(source->expected, source->expected_bytes);
  output->expected_bytes = source->expected_bytes;
  if (output->name == NULL || (output->input_units > 0 && output->input == NULL)
      || output->options == NULL || output->expected == NULL) {
    free_legacy_case(output);
    return 0;
  }
  return 1;
}

static int different_inputs(const LegacyCase *left, const LegacyCase *right) {
  return left->input_units != right->input_units
    || memcmp(left->input, right->input, left->input_units * sizeof(*left->input)) != 0;
}

static int open_kernel(const char *hot_path, IchiranKernel **output) {
  size_t hot_bytes = 0;
  uint8_t *hot = read_file(hot_path, &hot_bytes);
  if (hot == NULL) return 0;
  IchiranResult result = ichiran_kernel_open(hot, hot_bytes, output);
  free(hot);
  const int passed = result.status == ICHIRAN_OK && *output != NULL;
  if (!passed) {
    fputs("C product kernel open failed: ", stderr);
    print_buffer(&result.buffer);
    fputc('\n', stderr);
  }
  ichiran_buffer_free(result.buffer);
  return passed;
}

static int open_details(
  const char *details_path,
  FILE **file_output,
  IchiranDetailStore **store_output
) {
  FILE *file = fopen(details_path, "rb");
  size_t total_bytes = 0;
  uint8_t header[DETAIL_HEADER_BYTES];
  if (file == NULL || !file_length(file, &total_bytes)
      || fread(header, 1, sizeof(header), file) != sizeof(header)) {
    if (file != NULL) fclose(file);
    return 0;
  }
  size_t prefix_bytes = 0;
  IchiranResult length = ichiran_detail_prefix_length(
    header, sizeof(header), total_bytes, &prefix_bytes
  );
  if (length.status != ICHIRAN_OK || prefix_bytes < sizeof(header)) {
    ichiran_buffer_free(length.buffer);
    fclose(file);
    return 0;
  }
  ichiran_buffer_free(length.buffer);
  uint8_t *prefix = malloc(prefix_bytes);
  if (prefix == NULL || fseek(file, 0, SEEK_SET) != 0
      || fread(prefix, 1, prefix_bytes, file) != prefix_bytes) {
    free(prefix);
    fclose(file);
    return 0;
  }
  IchiranResult opened = ichiran_detail_store_open(
    prefix, prefix_bytes, total_bytes, store_output
  );
  free(prefix);
  const int passed = opened.status == ICHIRAN_OK && *store_output != NULL;
  ichiran_buffer_free(opened.buffer);
  if (!passed) {
    fclose(file);
    return 0;
  }
  *file_output = file;
  return 1;
}

static int legacy_exact(
  const IchiranKernel *kernel,
  const IchiranDetailStore *details,
  FILE *details_file,
  const LegacyCase *test,
  int corrupt_once,
  size_t *misses_output,
  size_t *corruption_rejections_output
) {
  IchiranLegacyOperation *operation = NULL;
  IchiranResult begun = ichiran_kernel_legacy_begin_utf16(
    kernel, test->input, test->input_units, test->options, test->options_bytes,
    NULL, 0, &operation
  );
  int passed = begun.status == ICHIRAN_OK && operation != NULL;
  ichiran_buffer_free(begun.buffer);
  if (!passed) return 0;

  uint32_t supplied_entry = ICHIRAN_NO_DETAIL;
  uint8_t *supplied = NULL;
  size_t supplied_bytes = 0;
  size_t misses = 0;
  size_t corruption_rejections = 0;
  int ready = 0;
  for (size_t step_index = 0; passed && step_index < 4096; step_index++) {
    IchiranStepResult step = ichiran_kernel_legacy_step(
      kernel, operation, details, supplied_entry, supplied, supplied_bytes
    );
    free(supplied);
    supplied = NULL;
    supplied_bytes = 0;
    supplied_entry = ICHIRAN_NO_DETAIL;
    if (step.status != ICHIRAN_OK) {
      fprintf(stderr, "legacy step failed for %s: ", test->name);
      print_buffer(&step.buffer);
      fputc('\n', stderr);
      ichiran_buffer_free(step.buffer);
      passed = 0;
      break;
    }
    if (step.state == ICHIRAN_STEP_READY) {
      passed = exact_buffer(&step.buffer, test->expected, test->expected_bytes);
      if (!passed) {
        size_t difference = 0;
        while (difference < test->expected_bytes && difference < step.buffer.byte_length
            && test->expected[difference] == step.buffer.data[difference]) difference++;
        fprintf(stderr, "detailed C parity mismatch %s (expected=%zu actual=%zu)\nexpected: ",
          test->name, test->expected_bytes, step.buffer.byte_length);
        const size_t start = difference > 200 ? difference - 200 : 0;
        fprintf(stderr, "first difference at %zu\nexpected: ", difference);
        fwrite(test->expected + start, 1,
          test->expected_bytes - start < 600 ? test->expected_bytes - start : 600, stderr);
        fputs("\nactual:   ", stderr);
        fwrite(step.buffer.data + start, 1,
          step.buffer.byte_length - start < 600 ? step.buffer.byte_length - start : 600, stderr);
        fputc('\n', stderr);
      }
      ichiran_buffer_free(step.buffer);
      ready = passed;
      break;
    }
    if (step.state != ICHIRAN_STEP_MISSING_DETAIL || step.entry_index == ICHIRAN_NO_DETAIL
        || step.range.byte_length == 0 || step.buffer.byte_length != 0) {
      ichiran_buffer_free(step.buffer);
      passed = 0;
      break;
    }
    ichiran_buffer_free(step.buffer);
    misses++;
    if (!read_range(details_file, step.range.offset, step.range.byte_length, &supplied)) {
      passed = 0;
      break;
    }
    supplied_bytes = step.range.byte_length;
    supplied_entry = step.entry_index;
    if (corrupt_once && misses == 1) {
      uint8_t *corrupt = copy_bytes(supplied, supplied_bytes);
      if (corrupt == NULL) {
        passed = 0;
        break;
      }
      corrupt[0] ^= 0xffu;
      IchiranStepResult rejected = ichiran_kernel_legacy_step(
        kernel, operation, details, supplied_entry, corrupt, supplied_bytes
      );
      free(corrupt);
      passed = rejected.status == ICHIRAN_CORRUPT_BLOCK
        && rejected.state == ICHIRAN_STEP_ERROR;
      if (passed) corruption_rejections++;
      ichiran_buffer_free(rejected.buffer);
    }
  }
  free(supplied);
  ichiran_legacy_operation_free(operation);
  if (misses_output != NULL) *misses_output = misses;
  if (corruption_rejections_output != NULL) {
    *corruption_rejections_output = corruption_rejections;
  }
  return passed && ready;
}

static int romanization_exact(
  const IchiranKernel *kernel,
  const char *name,
  char *hex,
  const uint8_t *options,
  size_t options_bytes,
  const uint8_t *method,
  size_t method_bytes,
  const uint8_t *expected,
  size_t expected_bytes
) {
  uint16_t *input = NULL;
  size_t input_units = 0;
  if (!parse_utf16(hex, &input, &input_units)) return 0;
  IchiranResult result = ichiran_kernel_romanize_utf16(
    kernel, input, input_units, options, options_bytes, method, method_bytes
  );
  free(input);
  const int passed = result.status == ICHIRAN_OK
    && exact_buffer(&result.buffer, expected, expected_bytes);
  if (!passed) fprintf(stderr, "romanization C parity mismatch %s\n", name);
  ichiran_buffer_free(result.buffer);
  return passed;
}

static int describe_exact(
  const IchiranDetailStore *details,
  FILE *details_file,
  uint32_t entry_index,
  const uint8_t *expected,
  size_t expected_bytes,
  int corrupt_once,
  size_t *corruption_rejections_output
) {
  IchiranDetailRange range;
  IchiranResult ranged = ichiran_detail_store_range(details, entry_index, &range);
  int passed = ranged.status == ICHIRAN_OK && range.byte_length > 0;
  ichiran_buffer_free(ranged.buffer);
  uint8_t *compressed = NULL;
  if (!passed || !read_range(details_file, range.offset, range.byte_length, &compressed)) return 0;
  if (corrupt_once) {
    compressed[0] ^= 0xffu;
    IchiranResult rejected = ichiran_detail_store_decode(
      details, entry_index, compressed, range.byte_length
    );
    passed = rejected.status == ICHIRAN_CORRUPT_BLOCK;
    if (passed && corruption_rejections_output != NULL) {
      (*corruption_rejections_output)++;
    }
    ichiran_buffer_free(rejected.buffer);
    compressed[0] ^= 0xffu;
  }
  IchiranResult decoded = ichiran_detail_store_decode(
    details, entry_index, compressed, range.byte_length
  );
  free(compressed);
  passed = passed && decoded.status == ICHIRAN_OK
    && exact_buffer(&decoded.buffer, expected, expected_bytes);
  ichiran_buffer_free(decoded.buffer);
  return passed;
}

static void *legacy_concurrently(void *context) {
  LegacyThread *thread = context;
  FILE *details_file = fopen(thread->details_path, "rb");
  thread->passed = details_file != NULL;
  for (size_t index = 0; thread->passed && index < CONCURRENT_REPEATS; index++) {
    thread->passed = legacy_exact(
      thread->kernel, thread->details, details_file, thread->first, 0, NULL, NULL
    ) && legacy_exact(
      thread->kernel, thread->details, details_file, thread->second, 0, NULL, NULL
    );
  }
  if (details_file != NULL) fclose(details_file);
  return NULL;
}

static int parse_legacy(char *line, LegacyCase *output) {
  char *name = strchr(line, '\t');
  if (name == NULL) return 0;
  *name++ = '\0';
  char *hex = strchr(name, '\t');
  if (hex == NULL) return 0;
  *hex++ = '\0';
  char *options = strchr(hex, '\t');
  if (options == NULL) return 0;
  *options++ = '\0';
  char *expected = strchr(options, '\t');
  if (expected == NULL) return 0;
  *expected++ = '\0';
  memset(output, 0, sizeof(*output));
  output->name = copy_bytes(name, strlen(name) + 1);
  output->options_bytes = strlen(options);
  output->options = copy_bytes(options, output->options_bytes);
  output->expected_bytes = strlen(expected);
  output->expected = copy_bytes(expected, output->expected_bytes);
  if (output->name == NULL || output->options == NULL || output->expected == NULL
      || !parse_utf16(hex, &output->input, &output->input_units)) {
    free_legacy_case(output);
    return 0;
  }
  return 1;
}

static int metadata_valid(const char *line) {
  const int immutable_pack = strstr(line, "\"mode\":\"immutable-baseline\"") != NULL
    && strstr(line, "\"currentLisp\":401") != NULL
    && strstr(line, "\"fallback\":301") != NULL
    && strstr(line, "\"canonicalTies\":{\"currentLisp\":3,\"fallback\":1,\"total\":4") != NULL
    && strstr(line, "\"names\":[\"cli:169\",\"cli:214\",\"hard:10\",\"probes:26\"]") != NULL
    && strstr(line, "61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0") != NULL
    && strstr(line, "0fc45731d84fbb7c2ccf3ef5692d2f1ab01e538325f0ed50135da38e621aa151") != NULL;
  const int same_pack = strstr(line, "\"mode\":\"same-pack\"") != NULL
    && strstr(line, "\"samePack\":702") != NULL
    && strstr(line, "\"canonicalTies\":0") != NULL
    && strstr(line, "\"hotSha256\":\"") != NULL
    && strstr(line, "\"detailsSha256\":\"") != NULL
    && strstr(line, "\"packVersion\":\"") != NULL
    && strstr(line, "\"sourceCommit\":\"") != NULL
    && strstr(line, "\"sourcesLockSha256\":\"") != NULL;
  return (immutable_pack || same_pack)
    && strstr(line, "\"format\":\"ichiran-c-product-v1\"") != NULL
    && strstr(line, "\"operations\":705") != NULL
    && strstr(line, "\"utf16\":3") != NULL
    && strstr(line, "\"romanization\":{\"operations\":8,\"retained\":5,\"utf16\":3}") != NULL
    && strstr(line, "\"describe\":4") != NULL;
}

static int verify_owned_product_errors(
  const IchiranKernel *kernel,
  const IchiranDetailStore *details
) {
  IchiranDetailRange range;
  IchiranResult missing_store = ichiran_detail_store_range(NULL, 0, &range);
  int passed = missing_store.status == ICHIRAN_INVALID_INPUT
    && missing_store.buffer.byte_length > 0;
  ichiran_buffer_free(missing_store.buffer);

  IchiranStepResult missing_operation = ichiran_kernel_legacy_step(
    kernel, NULL, details, ICHIRAN_NO_DETAIL, NULL, 0
  );
  passed = passed && missing_operation.status == ICHIRAN_INVALID_INPUT
    && missing_operation.state == ICHIRAN_STEP_ERROR
    && missing_operation.buffer.byte_length > 0;
  ichiran_buffer_free(missing_operation.buffer);

  static const uint8_t options[] =
    "{\"limit\":1,\"entities\":[],\"normalizePunctuation\":true}";
  static const uint8_t method[] = "unsupported";
  IchiranResult invalid_method = ichiran_kernel_romanize_utf16(
    kernel, NULL, 0, options, sizeof(options) - 1, method, sizeof(method) - 1
  );
  passed = passed && invalid_method.status == ICHIRAN_INVALID_INPUT
    && invalid_method.buffer.byte_length > 0;
  ichiran_buffer_free(invalid_method.buffer);
  return passed;
}

int main(int argc, char **argv) {
  if (argc != 3) {
    fputs("usage: c_product_harness <hot.bin> <details.bin>\n", stderr);
    return 2;
  }
  if (ichiran_kernel_abi_version() != ICHIRAN_KERNEL_ABI_VERSION) return 3;
  IchiranKernel *kernel = NULL;
  IchiranDetailStore *details = NULL;
  FILE *details_file = NULL;
  if (!open_kernel(argv[1], &kernel) || !open_details(argv[2], &details_file, &details)) {
    ichiran_kernel_free(kernel);
    return 4;
  }

  char *line = NULL;
  size_t capacity = 0;
  ssize_t length;
  size_t detailed = 0;
  size_t romanization = 0;
  size_t described = 0;
  size_t corrupt_recoveries = 0;
  int metadata = 0;
  int same_pack = 0;
  int passed = verify_owned_product_errors(kernel, details);
  LegacyCase concurrent[2] = {0};
  while (passed && (length = getline(&line, &capacity, stdin)) >= 0) {
    while (length > 0 && (line[length - 1] == '\n' || line[length - 1] == '\r')) {
      line[--length] = '\0';
    }
    if (line[0] == '#') {
      passed = !metadata && metadata_valid(line);
      same_pack = strstr(line, "\"mode\":\"same-pack\"") != NULL;
      metadata = 1;
      continue;
    }
    if (line[0] == 'L' && line[1] == '\t') {
      LegacyCase test;
      passed = parse_legacy(line, &test);
      size_t misses = 0;
      size_t corruption_rejections = 0;
      if (passed) passed = legacy_exact(
        kernel, details, details_file, &test, corrupt_recoveries == 0, &misses,
        &corruption_rejections
      );
      corrupt_recoveries += corruption_rejections;
      if (passed && misses > 0 && concurrent[0].name == NULL) {
        passed = copy_legacy_case(&test, &concurrent[0]);
      } else if (passed && misses > 0 && concurrent[1].name == NULL
          && different_inputs(&concurrent[0], &test)) {
        passed = copy_legacy_case(&test, &concurrent[1]);
      }
      free_legacy_case(&test);
      detailed++;
      continue;
    }
    if (line[0] == 'R' && line[1] == '\t') {
      char *name = line + 2;
      char *hex = strchr(name, '\t');
      char *options = hex == NULL ? NULL : strchr(hex + 1, '\t');
      char *method = options == NULL ? NULL : strchr(options + 1, '\t');
      char *expected = method == NULL ? NULL : strchr(method + 1, '\t');
      if (hex == NULL || options == NULL || method == NULL || expected == NULL) {
        passed = 0;
        continue;
      }
      *hex++ = *options++ = *method++ = *expected++ = '\0';
      passed = romanization_exact(
        kernel, name, hex, (uint8_t *)options, strlen(options),
        (uint8_t *)method, strlen(method), (uint8_t *)expected, strlen(expected)
      );
      romanization++;
      continue;
    }
    if (line[0] == 'D' && line[1] == '\t') {
      char *name = line + 2;
      char *index_text = strchr(name, '\t');
      char *expected = index_text == NULL ? NULL : strchr(index_text + 1, '\t');
      if (index_text == NULL || expected == NULL) {
        passed = 0;
        continue;
      }
      *index_text++ = *expected++ = '\0';
      errno = 0;
      char *end = NULL;
      const unsigned long entry_index = strtoul(index_text, &end, 10);
      passed = errno == 0 && *index_text != '\0' && *end == '\0' && entry_index <= UINT32_MAX
        && describe_exact(
          details, details_file, (uint32_t)entry_index,
          (uint8_t *)expected, strlen(expected), described == 0, &corrupt_recoveries
        );
      if (!passed) fprintf(stderr, "describe C parity mismatch %s\n", name);
      described++;
      continue;
    }
    passed = 0;
  }
  free(line);
  passed = passed && metadata && detailed == DETAILED_CASES
    && romanization == ROMANIZATION_CASES && described == DESCRIBE_CASES
    && corrupt_recoveries == 2
    && concurrent[0].name != NULL && concurrent[1].name != NULL;

  pthread_t threads[THREAD_COUNT];
  LegacyThread contexts[THREAD_COUNT];
  size_t started = 0;
  for (; passed && started < THREAD_COUNT; started++) {
    contexts[started] = (LegacyThread){
      .kernel = kernel,
      .details = details,
      .details_path = argv[2],
      .first = &concurrent[0],
      .second = &concurrent[1],
      .passed = 0
    };
    if (pthread_create(&threads[started], NULL, legacy_concurrently, &contexts[started]) != 0) {
      passed = 0;
      break;
    }
  }
  for (size_t index = 0; index < started; index++) {
    if (pthread_join(threads[index], NULL) != 0 || !contexts[index].passed) passed = 0;
  }

  free_legacy_case(&concurrent[0]);
  free_legacy_case(&concurrent[1]);
  fclose(details_file);
  ichiran_detail_store_free(details);
  ichiran_kernel_free(kernel);
  if (!passed) return 5;
  if (same_pack) {
    printf(
      "C ABI v3 same-pack product harness passed: detailed=702 utf16_detailed=3 "
      "romanization=5 utf16_romanization=3 describe=4 "
      "corrupt_recovery=%zu owned_errors=3 concurrent_detailed=32\n",
      corrupt_recoveries
    );
  } else {
    printf(
      "C ABI v3 product harness passed: detailed=702 utf16_detailed=3 current_lisp=401 fallback=301 "
      "authority_canonical_ties=4(current_lisp=3 fallback=1) romanization=5 "
      "utf16_romanization=3 describe=4 "
      "corrupt_recovery=%zu owned_errors=3 concurrent_detailed=32\n",
      corrupt_recoveries
    );
  }
  return 0;
}
