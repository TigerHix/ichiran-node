import { analyze } from './packages/grammar/src/engine/compiler.js';

const sentences = [
  '一緒にたべにいってほしいんです。',
  'もう一回説明してほしいです。',
  '車できてほしかった。',
  '一緒に勉強してほしいです。',
  'コーラを買ってほしいです。',
  '来てほしい。',
];

for (const sentence of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log(`Sentence: ${sentence}`);
  console.log('='.repeat(80));
  const result = await analyze(sentence);
  console.log(JSON.stringify(result, null, 2));
}
