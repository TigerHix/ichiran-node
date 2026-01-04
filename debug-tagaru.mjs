// Debug script to analyze how GiNZA parses たがる
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const engine = await useSharedEngine([]);

const testSentences = [
  '彼は動物園に行きたがる。',
  '彼女は最近の映画を見たがる。',
  '皆はポップコーンを食べたがると思う？',
];

for (const sentence of testSentences) {
  console.log('\n=== ' + sentence + ' ===\n');
  const doc = await engine.analyze(sentence);
  console.log(JSON.stringify(doc, null, 2));
}
