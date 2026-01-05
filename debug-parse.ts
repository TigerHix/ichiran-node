import { GrammarEngine } from './packages/grammar/src/program.js';
import { BUNPRO_JLPT2 } from './packages/grammar/src/rules/bunpro/jlpt2/index.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';

async function debug() {
  const client = new GinzaClient();
  await client.start();
  const engine = await GrammarEngine.create([BUNPRO_JLPT2], { client });

  const testSentences = [
    'できっこない。',
    'わかりっこない。',
    '勝てっこない。',
    '読めっこない。',
  ];

  for (const sentence of testSentences) {
    console.log('\n=== Analyzing:', sentence, '===');
    const docs = await client.analyze([sentence]);
    console.log(JSON.stringify(docs[0], null, 2));
  }

  await client.stop();
}

debug().catch(console.error);
