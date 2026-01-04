import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT2 } from './packages/grammar/src/rules/bunpro/jlpt2/index.js';

async function analyze() {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  const sentence = '今日は仕事に遅刻して部長に怒られたし、おまけに取引先の人も怒らせちゃったから、今日は最悪の日だったよ。';

  console.log('Analyzing:', sentence);
  console.log('');

  const doc = await engine.get().analyze(sentence);

  for (const token of doc.tokens) {
    console.log(`Text: "${token.text}" Lemma: "${token.lemma}" POS: ${token.pos} DEP: ${token.dep}`);
  }
}

analyze().catch(console.error);
