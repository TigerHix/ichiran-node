import { GiNZAEngine } from './packages/grammar/src/engine/ginza/index.ts';

async function analyze() {
  const engine = new GiNZAEngine();
  const sentence = '今日は仕事に遅刻して部長に怒られたし、おまけに取引先の人も怒らせちゃったから、今日は最悪の日だったよ。';

  console.log('Analyzing:', sentence);
  console.log('');

  const doc = await engine.parse(sentence);

  for (const token of doc.tokens) {
    console.log(`Text: "${token.text}" Lemma: "${token.lemma}" POS: ${token.pos} DEP: ${token.dep}`);
  }
}

analyze().catch(console.error);
