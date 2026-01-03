/**
 * Benchmark: measure grammar matching speed (assumes GiNZA warmed up)
 *
 * Usage: bun scripts/benchmark.ts
 */

import { GrammarEngine } from '../src/program.js';
import { BUNPRO_RULESETS } from '../src/rules/bunpro/index.js';

const TEST_SENTENCES = [
  '彼は絶対に諦めようとしない。',
  '日本に留学してこそ、日本語の難しさが分かる。',
  '責任をとってこそ、真のリーダーだ。',
  'じゃあ、行きましょう。',
  '食べてしまった。',
  'これは何なんですか。',
  '明日行くつもりだ。',
  '彼女は勉強だけでなくスポーツも得意だ。',
];

async function main() {
  console.log('Loading grammar engine with all Bunpro rulesets...');
  const engine = await GrammarEngine.create(BUNPRO_RULESETS, { ginza: { python: 'python3' } });

  const totalRules = BUNPRO_RULESETS.reduce((sum, rs) => sum + rs.rules.length, 0);
  console.log(`Loaded ${BUNPRO_RULESETS.length} rulesets with ${totalRules} total rules\n`);

  // Warm up GiNZA
  console.log('Warming up GiNZA...');
  await engine.match('テスト文です。');
  await engine.match('これはウォームアップです。');
  console.log('GiNZA warmed up.\n');

  // Benchmark: measure matching time only (excluding GiNZA parse)
  console.log('=== Benchmark: Full pipeline (GiNZA + matching) ===\n');

  const iterations = 10;
  const results: { sentence: string; avgMs: number; hits: number }[] = [];

  for (const sentence of TEST_SENTENCES) {
    const times: number[] = [];
    let hitCount = 0;

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      const hits = await engine.match(sentence);
      const elapsed = performance.now() - start;
      times.push(elapsed);
      hitCount = hits.length;
    }

    const avgMs = times.reduce((a, b) => a + b, 0) / times.length;
    results.push({ sentence, avgMs, hits: hitCount });
  }

  for (const r of results) {
    console.log(`${r.avgMs.toFixed(2).padStart(7)} ms | ${r.hits} hits | ${r.sentence.slice(0, 40)}`);
  }

  const overallAvg = results.reduce((sum, r) => sum + r.avgMs, 0) / results.length;
  console.log(`\nAverage: ${overallAvg.toFixed(2)} ms per sentence (full pipeline)\n`);

  // Benchmark: matching only (pre-parsed)
  console.log('=== Benchmark: Matching only (pre-parsed doc) ===\n');

  const matchOnlyResults: { sentence: string; avgMs: number }[] = [];
  const matchIterations = 100;

  for (const sentence of TEST_SENTENCES) {
    // Pre-parse
    const doc = await engine.analyze(sentence);

    const times: number[] = [];
    for (let i = 0; i < matchIterations; i++) {
      const start = performance.now();
      engine.matchDoc(doc, sentence);
      const elapsed = performance.now() - start;
      times.push(elapsed);
    }

    const avgMs = times.reduce((a, b) => a + b, 0) / times.length;
    matchOnlyResults.push({ sentence, avgMs });
  }

  for (const r of matchOnlyResults) {
    console.log(`${r.avgMs.toFixed(3).padStart(8)} ms | ${r.sentence.slice(0, 40)}`);
  }

  const matchOnlyAvg = matchOnlyResults.reduce((sum, r) => sum + r.avgMs, 0) / matchOnlyResults.length;
  console.log(`\nAverage: ${matchOnlyAvg.toFixed(3)} ms per sentence (matching only, ${totalRules} rules)\n`);

  await engine.close();
}

main().catch(console.error);

