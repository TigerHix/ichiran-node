import { GrammarEngine } from './packages/grammar/src/program.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const client = new GinzaClient();
await client.start();
const engine = await GrammarEngine.create([], { client });

const sentence = '何をなさろうとするのですか。';
console.log('SENTENCE:', sentence);
const result = await engine.analyze(sentence);
if (result && result.sentences && result.sentences.length > 0) {
  for (const token of result.sentences[0].tokens) {
    const info = "  " + token.text.padEnd(15) + " " + token.pos.padEnd(10) + " " + token.lemma.padEnd(15) + " inflectionForm=" + (token.inflectionForm || '-');
    console.log(info);
  }
}

await client.stop();
