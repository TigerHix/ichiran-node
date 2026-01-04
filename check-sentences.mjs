import { loadTestItem } from '/home/tiger/ichiran-node/packages/grammar/src/rules/bunpro/_test/helpers.js';

const item = loadTestItem('かと思ったら-かと思うと', 'JLPT2');
console.log('Rule ID:', item.id);
console.log('Sentences:', item.sentences.length);
console.log('\nFirst 5 sentences:');
for (let i = 0; i < Math.min(5, item.sentences.length); i++) {
  const s = item.sentences[i];
  console.log((i + 1) + '. "' + s.sentence + '"');
  console.log('   Answer: "' + s.answer + '"');
}
