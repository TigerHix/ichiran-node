import { describe } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.ts';
import { BUNPRO_JLPT4 } from './src/rules/bunpro/jlpt4/index.js';

const engine = useSharedEngine([BUNPRO_JLPT4]);
const eng = await engine.get();

const sentences = [
  '座るところがここしかありません。',
  'お茶しかないけど、よかったらどうぞ。',
  '「はい」と「いいえ」しかいわない人とは話しにくい。',
  '多く見えるけど、ちょっとしかない。',
  'ここに牛乳しか残っていない。',
];

for (const sentence of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('SENTENCE:', sentence);
  console.log('='.repeat(80));

  const result = await eng.explainMatch(sentence, 'しか-ない');

  console.log('Matched:', result.matched);
  if (!result.matched) {
    console.log('Reason:', result.reason);
    console.log('Partial bindings:', JSON.stringify(result.partialBinding, null, 2));
    console.log('Failed clause:', JSON.stringify(result.failedClause, null, 2));
  }

  // Also show the full parse
  const doc = await eng.analyze(sentence);
  console.log('\nTokens:');
  doc.tokens.forEach((tok, i) => {
    console.log(`  [${i}] "${tok.text}" POS=${tok.pos} tag=${tok.tag} lemma=${tok.lemma} inf=${tok.inflectionForm || '-'} dep=${tok.dep}(${tok.head})`);
  });
}
