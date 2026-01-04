import { analyze } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const sentences = [
  '私は子供のころにお医者さんに命を救われた。それで医者になろうと思った。',
  '彼は電車の中で痴漢をした。それで彼は警察に捕まった。',
];

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await analyze(sent);
  
  // Find それで
  for (let i = 0; i < doc.tokens.length; i++) {
    const token = doc.tokens[i];
    if (token.text === 'それ' || token.text === 'で') {
      console.log(`Token ${i}: "${token.text}" pos=${token.pos} lemma=${token.lemma} dep=${token.dep} head=${token.head}`);
    }
  }
}
