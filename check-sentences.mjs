import { loadBunproGrammarItemWithOptions } from './packages/grammar/src/data/bunpro/loader.js';

const item = loadBunproGrammarItemWithOptions('./packages/grammar/data/bunpro/JLPT5/い-adjective-noun.json', 'JLPT5', { allowTrivialSlug: true });

console.log('All sentences:');
item.sentences.forEach(s => {
  console.log('  ', s.sentence);
});

console.log('\nSentences with problematic kanji:');
item.sentences.forEach(s => {
  if (s.sentence.includes('怖') || s.sentence.includes('速') || s.sentence.includes('ふる') || s.sentence.includes('さむ') || s.sentence.includes('かっこ')) {
    console.log('  ', s.sentence);
  }
});
