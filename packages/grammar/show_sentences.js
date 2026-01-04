import { loadBunproGrammarItemWithOptions } from './dist/data/bunpro/loader.js';

const item = loadBunproGrammarItemWithOptions('data/bunpro/JLPT4/各.json', 'JLPT4', { allowTrivialSlug: true });
item.sentences.forEach((s, idx) => {
  console.log((idx + 1) + '. "' + s.sentence + '"');
});
