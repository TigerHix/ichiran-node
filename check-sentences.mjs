import { loadBunproGrammarItemWithOptions } from './packages/grammar/src/data/bunpro/loader.ts';
import { join } from 'node:path';

const item = loadBunproGrammarItemWithOptions(
  join('./packages/grammar/data/bunpro/JLPT4/そんなに.json'),
  'JLPT4',
  { allowTrivialSlug: true }
);

console.log('Total sentences:', item.sentences.length);
console.log('\nSentences loaded:');
for (const { sentence, answer } of item.sentences) {
  console.log(`  [${answer}] ${sentence}`);
}
