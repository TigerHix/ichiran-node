// ichiran/dict/profiling - Performance instrumentation and profiling
// Extracted from dict.ts for modularity
// Enable with: ICHIRAN_PROFILE=1 or ICHIRAN_PROFILE=true

// =============================================================================
// PERFORMANCE INSTRUMENTATION
// =============================================================================

const ENABLE_PROFILING = process.env.ICHIRAN_PROFILE === '1' || process.env.ICHIRAN_PROFILE === 'true';

export const PERF_COUNTERS = {
  findWordFull: { calls: 0, time: 0 },
  findWord: { calls: 0, time: 0 },
  findWordSuffix: { calls: 0, time: 0 },
  findWordAsHiragana: { calls: 0, time: 0 },
  findCounter: { calls: 0, time: 0 },
  genScore: { calls: 0, time: 0 },
  calcScore: { calls: 0, time: 0 },
  calcScore_entryQuery: { calls: 0, time: 0 },
  calcScore_wordConjData: { calls: 0, time: 0 },
  calcScore_sensePropQuery: { calls: 0, time: 0 },
  calcScore_isArch: { calls: 0, time: 0 },
  calcScore_getNonArchPosi: { calls: 0, time: 0 },
  calcScore_getOriginalText: { calls: 0, time: 0 },
  wordConjData: { calls: 0, time: 0 },
  getOriginalText: { calls: 0, time: 0 },
  joinSubstringWords: { calls: 0, time: 0 },
  joinSubstringWords_getSuffixMap: { calls: 0, time: 0 },
  joinSubstringWords_findWord: { calls: 0, time: 0 },
  findSubstringWords: { calls: 0, time: 0 },
  findBestPath: { calls: 0, time: 0 },
  fillSegmentPath: { calls: 0, time: 0 },
};

// Inline profiling helper - no-op when profiling disabled
export function startTimer(counter: keyof typeof PERF_COUNTERS): () => void {
  if (!ENABLE_PROFILING) return () => {};

  const start = performance.now();
  PERF_COUNTERS[counter].calls++;

  return () => {
    PERF_COUNTERS[counter].time += performance.now() - start;
  };
}

export function resetPerfCounters() {
  if (!ENABLE_PROFILING) return;

  for (const key in PERF_COUNTERS) {
    PERF_COUNTERS[key as keyof typeof PERF_COUNTERS].calls = 0;
    PERF_COUNTERS[key as keyof typeof PERF_COUNTERS].time = 0;
  }
}

export function printPerfCountersAndReset() {
  if (!ENABLE_PROFILING) {
    console.log('Performance profiling is disabled. Enable with ICHIRAN_PROFILE=1');
    return;
  }

  console.log('\n' + '='.repeat(80));
  console.log('PERFORMANCE COUNTERS');
  console.log('='.repeat(80));

  const sorted = Object.entries(PERF_COUNTERS)
    .filter(([_, stats]) => stats.calls > 0)
    .sort((a, b) => b[1].time - a[1].time);

  for (const [name, stats] of sorted) {
    const avg = stats.calls > 0 ? stats.time / stats.calls : 0;
    console.log(`${name.padEnd(25)} ${stats.calls.toString().padEnd(8)} calls  ${stats.time.toFixed(2).padStart(10)}ms total  ${avg.toFixed(3).padStart(8)}ms avg`);
  }
  console.log('='.repeat(80) + '\n');

  // Reset counters
  resetPerfCounters();
}

export function isProfilingEnabled(): boolean {
  return ENABLE_PROFILING;
}
