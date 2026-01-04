import { GrammarEngine } from './packages/grammar/src/program.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';
import { linguisticRule } from './packages/grammar/src/engine/lang.js';

// Test simplest possible rule
const simpleRule = linguisticRule('なさる-simple', (r) => {
  const nasaru = r.tok({ lemma: 'なさる' }, 'nasaru');
  r.capture(nasaru);
});

const client = new GinzaClient();
await client.start();

const ruleset = {
  id: 'test',
  rules: [simpleRule],
};

const engine = await GrammarEngine.create([ruleset], { client});

const sentence = '平野さんは明日の飲み会に出席なさいますか。';
console.log('SENTENCE:', sentence);
console.log('\nTesting simple rule (just lemma)...');

const matches = await engine.match(sentence);
console.log('Matches:', JSON.stringify(matches, null, 2));

await client.stop();
