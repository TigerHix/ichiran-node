import { analyze } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const sentences = [
  'そういう人は嫌い。',
  'こういう人形がほしい。',
  'どういう意味ですか。',
  'ああいう車に乗ってみたい。',
];

for (const sent of sentences) {
  console.log(`\n=== ${sent} ===`);
  const doc = await analyze(sent);
  console.log(JSON.stringify(doc, null, 2));
}
