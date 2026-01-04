import { GrammarEngine } from './packages/grammar/src/program.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '座りなさい。',
  '早くしなさい。',
  '勉強しなさい。',
  '期待なさらないでください。',
  'お飲み物はどうなさいますか？',
];

const client = new GinzaClient();
await client.start();
const engine = await GrammarEngine.create([], { client });

for (const sentence of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('SENTENCE:', sentence);
  console.log('='.repeat(80));
  const result = await engine.analyze(sentence);
  if (!result || !result.sentences || result.sentences.length === 0) {
    console.log('  ERROR: No result');
    continue;
  }
  for (const token of result.sentences[0].tokens) {
    const info = "  " + token.text.padEnd(15) + " " + token.pos.padEnd(10) + " " + token.lemma.padEnd(15) + " inflectionForm=" + (token.inflectionForm || '-');
    console.log(info);
  }
}

await client.stop();
