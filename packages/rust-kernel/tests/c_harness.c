#define _POSIX_C_SOURCE 200809L

#include "ichiran_kernel.h"

#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define EXPECTED_CLEAN_CASES 1236u
#define EXPECTED_UTF16_CASES 3u
#define EXPECTED_CASES (EXPECTED_CLEAN_CASES + EXPECTED_UTF16_CASES)
#define THREAD_COUNT 4u
#define ANALYSES_PER_THREAD 16u

typedef struct ParityCase {
  char *name;
  uint16_t *input;
  size_t input_units;
  uint8_t *options;
  size_t options_bytes;
  uint8_t *expected;
  size_t expected_bytes;
} ParityCase;

typedef struct AnalyzeThread {
  const IchiranKernel *kernel;
  const ParityCase *first;
  const ParityCase *second;
  int passed;
} AnalyzeThread;

static uint8_t *read_file(const char *path, size_t *byte_length) {
  FILE *file = fopen(path, "rb");
  if (file == NULL) return NULL;
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
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

static void free_case(ParityCase *value) {
  free(value->name);
  free(value->input);
  free(value->options);
  free(value->expected);
}

static void free_cases(ParityCase *cases, size_t count) {
  for (size_t index = 0; index < count; index++) free_case(&cases[index]);
  free(cases);
}

static void *copy_bytes(const char *value, size_t byte_length) {
  if (byte_length == 0) return NULL;
  void *copy = malloc(byte_length);
  if (copy != NULL) memcpy(copy, value, byte_length);
  return copy;
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
  if (units > SIZE_MAX / sizeof(uint16_t)) return 0;
  uint16_t *result = malloc(units * sizeof(uint16_t));
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

static int parse_case(char *line, ParityCase *output) {
  char *name_end = strchr(line, '\t');
  if (name_end == NULL) return 0;
  *name_end = '\0';
  char *hex = name_end + 1;
  char *hex_end = strchr(hex, '\t');
  if (hex_end == NULL) return 0;
  *hex_end = '\0';
  char *options = hex_end + 1;
  char *options_end = strchr(options, '\t');
  if (options_end == NULL) return 0;
  *options_end = '\0';
  char *expected = options_end + 1;

  memset(output, 0, sizeof(*output));
  output->name = copy_bytes(line, strlen(line) + 1);
  output->options_bytes = strlen(options);
  output->options = copy_bytes(options, output->options_bytes);
  output->expected_bytes = strlen(expected);
  output->expected = copy_bytes(expected, output->expected_bytes);
  if (output->name == NULL || output->options == NULL || output->expected == NULL
      || !parse_utf16(hex, &output->input, &output->input_units)) {
    free_case(output);
    memset(output, 0, sizeof(*output));
    return 0;
  }
  return 1;
}

static int load_cases(FILE *input, ParityCase **output, size_t *output_count) {
  char *line = NULL;
  size_t line_capacity = 0;
  ssize_t line_length;
  ParityCase *cases = NULL;
  size_t count = 0;
  int saw_metadata = 0;
  while ((line_length = getline(&line, &line_capacity, input)) >= 0) {
    while (line_length > 0
        && (line[line_length - 1] == '\n' || line[line_length - 1] == '\r')) {
      line[--line_length] = '\0';
    }
    if (line_length == 0) continue;
    if (line[0] == '#') {
      const int immutable_pack = strstr(line, "\"mode\":\"immutable-baseline\"") != NULL
        && strstr(line, "\"hotSha256\":"
          "\"61f2882e086be7e0e1b6ba9000e76e0e735b22ea443146f628f04cf877ff6ae0\"")
          != NULL;
      const int same_pack = strstr(line, "\"mode\":\"same-pack\"") != NULL
        && strstr(line, "\"packVersion\":\"") != NULL
        && strstr(line, "\"sourceCommit\":\"") != NULL
        && strstr(line, "\"sourcesLockSha256\":\"") != NULL
        && strstr(line, "\"hotSha256\":\"") != NULL;
      if (saw_metadata || (!immutable_pack && !same_pack)
          || strstr(line, "\"format\":\"ichiran-c-parity-v1\"") == NULL
          || strstr(line, "\"operations\":1239") == NULL
          || strstr(line, "\"cleanOperations\":1236") == NULL
          || strstr(line, "\"utf16\":3") == NULL
          || strstr(line, "\"suites\":{\"segmentation\":534,\"cli\":252,"
            "\"hard\":149,\"counters\":200,\"entities\":54,\"probes\":47}") == NULL
          || strstr(line, "\"oracle\":\"frozen TypeScript") == NULL
          || strstr(line, "\"sourceRevision\":") == NULL) {
        free(line);
        free_cases(cases, count);
        return 0;
      }
      saw_metadata = 1;
      continue;
    }
    if (count >= EXPECTED_CASES) {
      free(line);
      free_cases(cases, count);
      return 0;
    }
    ParityCase *grown = realloc(cases, (count + 1) * sizeof(*cases));
    if (grown == NULL) {
      free(line);
      free_cases(cases, count);
      return 0;
    }
    cases = grown;
    if (!parse_case(line, &cases[count])) {
      fprintf(stderr, "invalid C parity row %zu\n", count + 1);
      free(line);
      free_cases(cases, count);
      return 0;
    }
    count++;
  }
  free(line);
  if (ferror(input) || !saw_metadata || count != EXPECTED_CASES) {
    fprintf(stderr, "C parity corpus has %zu cases; expected %u\n", count, EXPECTED_CASES);
    free_cases(cases, count);
    return 0;
  }
  *output = cases;
  *output_count = count;
  return 1;
}

static void print_buffer(const IchiranBuffer *buffer) {
  const size_t shown = buffer->byte_length < 600 ? buffer->byte_length : 600;
  if (shown > 0) fwrite(buffer->data, 1, shown, stderr);
  if (shown < buffer->byte_length) fputs("...", stderr);
}

static int analyze_exact(const IchiranKernel *kernel, const ParityCase *test) {
  IchiranResult result = ichiran_kernel_analyze_utf16(
    kernel,
    test->input,
    test->input_units,
    test->options,
    test->options_bytes
  );
  const int passed = result.status == ICHIRAN_OK
    && result.buffer.byte_length == test->expected_bytes
    && memcmp(result.buffer.data, test->expected, test->expected_bytes) == 0;
  if (!passed) {
    fprintf(stderr, "C parity mismatch %s (status=%u, expected=%zu, actual=%zu)\n",
      test->name, result.status, test->expected_bytes, result.buffer.byte_length);
    fputs("expected: ", stderr);
    fwrite(test->expected, 1, test->expected_bytes < 600 ? test->expected_bytes : 600, stderr);
    fputs("\nactual:   ", stderr);
    print_buffer(&result.buffer);
    fputc('\n', stderr);
  }
  ichiran_buffer_free(result.buffer);
  return passed;
}

static void *analyze_concurrently(void *context) {
  AnalyzeThread *thread = context;
  thread->passed = 1;
  for (size_t index = 0; index < ANALYSES_PER_THREAD; index++) {
    if (!analyze_exact(thread->kernel, thread->first)
        || !analyze_exact(thread->kernel, thread->second)) {
      thread->passed = 0;
      break;
    }
  }
  return NULL;
}

static int verify_owned_error_buffer(void) {
  static const uint8_t options[] =
    "{\"limit\":1,\"entities\":[],\"normalizePunctuation\":false}";
  IchiranResult result = ichiran_kernel_analyze_utf16(
    NULL, NULL, 0, options, sizeof(options) - 1
  );
  static const char expected[] =
    "{\"code\":\"invalid-input\",\"message\":\"kernel pointer is null\"}";
  const int passed = result.status == ICHIRAN_INVALID_INPUT
    && result.buffer.byte_length == sizeof(expected) - 1
    && memcmp(result.buffer.data, expected, sizeof(expected) - 1) == 0;
  ichiran_buffer_free(result.buffer);
  return passed;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: c_parity_corpus.ts <release-dir> | c_harness <hot.bin>\n");
    return 2;
  }
  if (ichiran_kernel_abi_version() != ICHIRAN_KERNEL_ABI_VERSION) return 3;

  ParityCase *cases = NULL;
  size_t case_count = 0;
  if (!load_cases(stdin, &cases, &case_count)) return 4;

  size_t hot_bytes = 0;
  uint8_t *hot = read_file(argv[1], &hot_bytes);
  if (hot == NULL) {
    free_cases(cases, case_count);
    return 5;
  }
  IchiranKernel *kernel = NULL;
  IchiranResult opened = ichiran_kernel_open(hot, hot_bytes, &kernel);
  free(hot);
  if (opened.status != ICHIRAN_OK || kernel == NULL) {
    fprintf(stderr, "C open failed (%u): ", opened.status);
    print_buffer(&opened.buffer);
    fputc('\n', stderr);
    ichiran_buffer_free(opened.buffer);
    free_cases(cases, case_count);
    return 6;
  }
  ichiran_buffer_free(opened.buffer);

  int passed = verify_owned_error_buffer();
  size_t exact = 0;
  for (; passed && exact < case_count; exact++) passed = analyze_exact(kernel, &cases[exact]);

  const ParityCase *thread_cases[2] = { NULL, NULL };
  for (size_t index = 0; index < case_count && thread_cases[1] == NULL; index++) {
    if (cases[index].input_units > 0 && cases[index].input_units <= 4) {
      if (thread_cases[0] == NULL) thread_cases[0] = &cases[index];
      else thread_cases[1] = &cases[index];
    }
  }
  if (thread_cases[1] == NULL) passed = 0;

  pthread_t threads[THREAD_COUNT];
  AnalyzeThread contexts[THREAD_COUNT];
  size_t started = 0;
  for (; passed && started < THREAD_COUNT; started++) {
    contexts[started].kernel = kernel;
    contexts[started].first = thread_cases[0];
    contexts[started].second = thread_cases[1];
    contexts[started].passed = 0;
    if (pthread_create(&threads[started], NULL, analyze_concurrently,
        &contexts[started]) != 0) {
      passed = 0;
      break;
    }
  }
  for (size_t index = 0; index < started; index++) {
    if (pthread_join(threads[index], NULL) != 0 || !contexts[index].passed) passed = 0;
  }

  ichiran_kernel_free(kernel);
  free_cases(cases, case_count);
  if (!passed) return 7;
  printf(
    "C ABI v3 clean harness passed: exact=%u utf16=%u total=%zu "
    "segmentation=534 cli=252 hard=149 counters=200 entities=54 probes=47 "
    "owned_errors=1 concurrent_exact=128\n",
    EXPECTED_CLEAN_CASES, EXPECTED_UTF16_CASES, exact
  );
  return 0;
}
