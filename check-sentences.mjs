import { loadBunproGrammarItemWithOptions } from './packages/grammar/src/data/bunpro/loader.js';
import { join } from 'node:path';

const filePath = join('packages/grammar/data/bunpro/JLPT4', '真(っ).json');
const item = loadBunproGrammarItemWithOptions(filePath, 'JLPT4', { allowTrivialSlug: true });

console.log('Rule:', item.id);
console.log('Total sentences:', item.sentences.length);
console.log('\nFirst 10 sentences:');
for (let i = 0; i < Math.min(10, item.sentences.length); i++) {
  console.log((i + 1) + '. ' + item.sentences[i].sentence);
}
