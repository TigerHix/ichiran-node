import { GrammarEngine } from './packages/grammar/src/program.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';
import nasaruRule from './packages/grammar/src/rules/bunpro/jlpt4/なさる.js';

const client = new GinzaClient();
await client.start();

const ruleset = {
  id: 'test',
  rules: [nasaruRule],
};

const engine = await GrammarEngine.create([ruleset], { client });

const sentence = '平野さんは明日の飲み会に出席なさいますか。';
console.log('SENTENCE:', sentence);
console.log('\nAnalyzing...');

const result = await engine.analyze(sentence);
if (result && result.sentences && result.sentences.length > 0) {
  for (const token of result.sentences[0].tokens) {
    const info = "  " + token.text.padEnd(15) + " " + token.pos.padEnd(10) + " " + token.lemma.padEnd(15) + " inflectionForm=" + (token.inflectionForm || '-');
    console.log(info);
  }
}

console.log('\nExplaining match:');
const explain = await engine.explainMatch(sentence, 'なさる');
console.log(JSON.stringify(explain, null, 2));

await client.stop();
