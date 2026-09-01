#include "ichiran_kernel.h"

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define THREAD_COUNT 4
#define ANALYSES_PER_THREAD 16

static uint8_t *read_file(const char *path, size_t *byte_length) {
  FILE *file = fopen(path, "rb");
  if (file == NULL) return NULL;
  if (fseek(file, 0, SEEK_END) != 0) return NULL;
  long length = ftell(file);
  if (length <= 0 || fseek(file, 0, SEEK_SET) != 0) return NULL;
  uint8_t *bytes = malloc((size_t)length);
  if (bytes == NULL || fread(bytes, 1, (size_t)length, file) != (size_t)length) return NULL;
  fclose(file);
  *byte_length = (size_t)length;
  return bytes;
}

static int contains(const IchiranBuffer *buffer, const char *text) {
  const size_t length = strlen(text);
  if (length > buffer->byte_length) return 0;
  for (size_t index = 0; index + length <= buffer->byte_length; index++) {
    if (memcmp(buffer->data + index, text, length) == 0) return 1;
  }
  return 0;
}

static int analyze(
  const IchiranKernel *kernel,
  const uint16_t *text,
  size_t units,
  const char *expected
) {
  IchiranResult result = ichiran_kernel_analyze_utf16(kernel, text, units, 1);
  const int passed = result.status == ICHIRAN_OK && contains(&result.buffer, expected);
  if (!passed) {
    fprintf(stderr, "C analysis failed (%u): %.*s\n", result.status,
      (int)result.buffer.byte_length, result.buffer.data);
  }
  ichiran_buffer_free(result.buffer);
  return passed;
}

typedef struct AnalyzeThread {
  const IchiranKernel *kernel;
  int passed;
} AnalyzeThread;

static void *analyze_concurrently(void *context) {
  AnalyzeThread *thread = context;
  const uint16_t cat[] = { 0x732b };
  const uint16_t ate[] = { 0x98df, 0x3079, 0x305f };
  thread->passed = 1;
  for (size_t index = 0; index < ANALYSES_PER_THREAD; index++) {
    if (!analyze(thread->kernel, cat, 1, "\"score\":19")
      || !analyze(thread->kernel, ate, 3, "\"score\":336")) {
      thread->passed = 0;
      break;
    }
  }
  return NULL;
}

int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: c_harness <hot.bin>\n");
    return 2;
  }
  if (ichiran_kernel_abi_version() != ICHIRAN_KERNEL_ABI_VERSION) return 3;
  size_t hot_bytes = 0;
  uint8_t *hot = read_file(argv[1], &hot_bytes);
  if (hot == NULL) return 4;
  IchiranKernel *kernel = NULL;
  IchiranResult opened = ichiran_kernel_open(hot, hot_bytes, &kernel);
  free(hot);
  if (opened.status != ICHIRAN_OK || kernel == NULL) {
    fprintf(stderr, "C open failed (%u): %.*s\n", opened.status,
      (int)opened.buffer.byte_length, opened.buffer.data);
    ichiran_buffer_free(opened.buffer);
    return 5;
  }
  ichiran_buffer_free(opened.buffer);

  const uint16_t cat[] = { 0x732b };
  const uint16_t ate[] = { 0x98df, 0x3079, 0x305f };
  const uint16_t forgot[] = { 0x5fd8, 0x308c, 0x305f };
  const uint16_t astral[] = { 0xd83d, 0xde00 };
  const uint16_t high[] = { 0xd83d };
  const uint16_t low[] = { 0xde00 };
  int passed = analyze(kernel, cat, 1, "\"score\":19")
    && analyze(kernel, ate, 3, "\"score\":336")
    && analyze(kernel, forgot, 3, "\"score\":216")
    && analyze(kernel, astral, 2, "\"end\":2")
    && analyze(kernel, high, 1, "\"input\":\"\\ud83d\"")
    && analyze(kernel, low, 1, "\"input\":\"\\ude00\"");

  pthread_t threads[THREAD_COUNT];
  AnalyzeThread contexts[THREAD_COUNT];
  size_t started = 0;
  for (; passed && started < THREAD_COUNT; started++) {
    contexts[started].kernel = kernel;
    contexts[started].passed = 0;
    if (pthread_create(&threads[started], NULL, analyze_concurrently,
        &contexts[started]) != 0) {
      passed = 0;
      break;
    }
  }
  for (size_t index = 0; index < started; index++) {
    if (pthread_join(threads[index], NULL) != 0 || !contexts[index].passed) {
      passed = 0;
    }
  }

  ichiran_kernel_free(kernel);
  if (!passed) return 6;
  puts("C ABI M1 harness passed: lexical=1 morphology=1 generated=1 UTF16=3 concurrent=128");
  return 0;
}
