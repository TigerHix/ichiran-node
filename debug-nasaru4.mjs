import { GrammarEngine } from './packages/grammar/src/program.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';
import { linguisticRule } from './packages/grammar/src/engine/lang.js';

// Test with inflectionForm
const simpleRule = linguisticRule('なさる-simple', (r) => {
  const nasaru = r.tok({ 
    lemma: 'なさる',
    posOneOf: ['VERB', 'AUX'],
    inflectionForm: ['連体形-一般', '未然形-一般', '連用形-イ音便', '連用形-促音便', '意志推量形'],
  }, 'nasaru');
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
console.log('\nTesting with inflectionForm...');

const matches = await engine.match(sentence);
console.log('Matches:', JSON.stringify(matches, null, 2));

await client.stop();
