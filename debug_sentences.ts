import { loadBunproGrammarItemWithOptions } from './packages/grammar/src/data/bunpro/loader.js';

const item = loadBunproGrammarItemWithOptions(
  './packages/grammar/data/bunpro/JLPT5/うverb--ない.json',
  'JLPT5',
  { allowTrivialSlug: true }
);

if (item) {
  console.log('ID:', item.id);
  console.log('Answer forms:', item.answerForms);
  console.log('\nSentences:');
  for (const s of item.sentences) {
    console.log(`  "${s.sentence}"`);
  }
} else {
  console.log('Failed to load');
}
