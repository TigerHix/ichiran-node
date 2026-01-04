import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], { ginza: { python: 'python3' } });

  const sentence = '「あの、すみません。どうして、あの人たちは、あそこにならんでいるのですか？」';

  console.log(`SENTENCE: ${sentence}`);
  console.log('='.repeat(100));
  const doc = await engine.analyze(sentence);
  if (doc && doc.sentences[0]) {
    const sent = doc.sentences[0];
    for (let i = 0; i < sent.tokens.length; i++) {
      const tok = sent.tokens[i];
      console.log(`  [${i}] ${tok.text.padEnd(12)} pos=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(8)} head=${tok.head} inflectionForm=${tok.inflectionForm || 'N/A'}`);
    }
  }

  await engine.close();
}

main();
