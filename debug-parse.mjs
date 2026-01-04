import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT4 } from './packages/grammar/src/rules/bunpro/jlpt4/index.js';

async function debug() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT4]);

  const sentence = '彼女はゆうがに踊る。';
  console.log(`=== ${sentence} ===`);
  const doc = await engine.analyze(sentence);
  const tokens = doc.sentences[0].tokens;

  for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    const tag = t.tag || 'none';
    const infl = t.inflectionForm || 'none';
    console.log(`[${i}] ${t.text}: pos=${t.pos} tag=${tag} inflection=${infl} lemma=${t.lemma} dep=${t.dep} head=${t.head}`);
  }

  await engine.close();
}

debug().catch(console.error);
