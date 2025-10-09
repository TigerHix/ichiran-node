#!/usr/bin/env tsx
/**
 * Profile script for /segment vs /analyze without starting servers.
 * Usage:
 *   GRAMMAR_PROFILE=1 tsx scripts/profile-analyze.ts
 */
import { romanizeStar } from '../packages/core/src/romanize.js';
import { transformRomanizeStarResult } from '../packages/core/src/presentation/transformers.js';
import { analyzeText } from '../packages/grammar/src/runtime.js';
import { grammarCatalog } from '../packages/grammar/src/catalog.js';

const SAMPLE = '広場へ向かう途中、町の案内板を見ました。祭りは誰でも参加できます。しかし、そのときはまだ誰も来ませんでした。私は強く願います。「いい夜になりますように」と。広場は星がいちばんよく見える場所です。この町は空が近いです。都会よりこの町の空のほうが星が多いのです。露店はすぐににぎやかになりますが、値段は高くないです。子どもたちは自由に走ります。ある子は上手に太鼓を叩きます。甘い匂いと温かい笑い声が、夜を優しくします。私はお守りを買いました。旅が安全になるように祈るのです。隣の老人は笑って言いました。「この川の水がいちばん冷たいです。夏でも驚くほどなんです。」私はうなずき、茶か珈琲か迷いましたが、結局、温かい茶にしました。体が軽くなります。';

function hr(label: string) {
  console.log('\n' + '='.repeat(80));
  console.log(label);
  console.log('='.repeat(80));
}

async function main() {
  hr('SEGMENT PROFILE (romanizeStar + transform)');
  const t0 = performance.now();
  const raw = await romanizeStar(SAMPLE, { limit: 5 });
  const t1 = performance.now();
  const seg = await transformRomanizeStarResult(raw);
  const t2 = performance.now();
  console.log({
    romanizeStarMs: (t1 - t0).toFixed(2),
    transformMs: (t2 - t1).toFixed(2),
    totalMs: (t2 - t0).toFixed(2),
  });

  hr('ANALYZE PROFILE (analyzeText)');
  const a0 = performance.now();
  const result = await analyzeText(SAMPLE, grammarCatalog, { maxMatches: 100, limit: 5 });
  const a1 = performance.now();
  console.log({
    analyzeTotalMs: (a1 - a0).toFixed(2),
    grammarMatches: result.grammarMatches.length,
    tokens: result.tokens.length,
  });
}

main().catch(err => {
  console.error(err);
  process.exit(1);
});


