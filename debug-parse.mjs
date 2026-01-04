import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './packages/grammar/src/rules/bunpro/jlpt3/index.js';

async function test() {
  const { get: engine } = useSharedEngine([BUNPRO_JLPT3]);
  const doc = await engine().analyze('面倒くさくても朝ご飯を食べることだ。');
  console.log('Tokens:');
  doc.tokens.forEach((t, i) => {
    console.log(`${i}: ${t.text} (lemma=${t.lemma}, pos=${t.pos}, inflection=${t.inflectionForm}, head=${t.head})`);
  });
}

test().catch(console.error);
