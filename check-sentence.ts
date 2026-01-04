import { loadBunproGrammarItemWithOptions } from './packages/grammar/src/data/bunpro/loader.js';

const item = loadBunproGrammarItemWithOptions('./packages/grammar/data/bunpro/JLPT5/ている2.json', 'JLPT5', { allowTrivialSlug: true });
const s = item.sentences.find((s: any) => s.sentence.includes('ならんでいる'));
console.log(JSON.stringify(s, null, 2));
