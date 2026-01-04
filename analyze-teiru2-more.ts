import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT5 } from './packages/grammar/src/rules/bunpro/jlpt5/index.js';

async function main() {
  const engine = await GrammarEngine.create([BUNPRO_JLPT5], { ginza: { python: 'python3' } });

  // More test sentences - negatives, casual forms, and polite forms
  const sentences = [
    'お前はもう死んでる。',  // casual: いる -> る
    'あの映画のタイトル、知ってる？',  // casual question
    'サスケさんは結婚していないでしょう？',  // negative
    'あのカバは太っていない。',  // negative
    'ななさんのバナナは腐っています。',  // polite
    'お母さんは今買い物に行っています。',  // polite with motion verb
  ];

  for (const sentence of sentences) {
    console.log('\n' + '='.repeat(100));
    console.log(`SENTENCE: ${sentence}`);
    console.log('='.repeat(100));
    const doc = await engine.analyze(sentence);
    if (doc && doc.sentences[0]) {
      const sent = doc.sentences[0];
      for (const tok of sent.tokens) {
        console.log(`  [${tok.id}] ${tok.text.padEnd(12)} pos=${tok.pos.padEnd(6)} lemma=${tok.lemma.padEnd(10)} dep=${tok.dep.padEnd(8)} head=${tok.head} inflectionForm=${tok.inflectionForm || 'N/A'}`);
      }
    }
  }

  await engine.close();
}

main();
