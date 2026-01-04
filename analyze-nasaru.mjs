import { GrammarEngine } from './packages/grammar/src/program.js';
import { GinzaClient } from './packages/grammar/src/ginza/client.js';

const sentences = [
  '平野さんは明日の飲み会に出席なさいますか。',
  'お飲み物はどうなさいますか？',
  '昨日何時まで仕事をなさったのですか。',
  '今何を勉強なさっていますか。',
  '社長は先週どちらでゴルフをなさったのですか。',
  '私はそんなに上手じゃないので、期待なさらないでください。',
  '結婚記念日にどうなさるつもりですか。',
  '週末もお仕事をなさるのですか。',
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
    const info = "  " + token.text.padEnd(15) + " " + token.pos.padEnd(10) + " " + token.lemma.padEnd(15) + " inflectionForm=" + (token.inflectionForm || '-') + " head=" + token.head;
    console.log(info);
  }
}

await client.stop();
