import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const engine = useSharedEngine([]);

const testSentences = [
  '書きなおす',
  '書きなおした',
  '書き直した',
  'やりなおす',
  'やり直す',
  'お客様に挨拶をし直す。',
  '電話を掛け直してください。',
  'この文章を書き直した方がいいよ。',
];

for (const sentence of testSentences) {
  console.log(`\n=== ${sentence} ===`);
  const doc = await engine.analyze(sentence);
  for (const token of doc.tokens) {
    console.log(`  ${token.text} | pos=${token.pos} | lemma=${token.lemma} | inflectionForm=${token.inflectionForm} | dep=${token.dep}`);
  }
}
