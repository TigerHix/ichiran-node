import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const testSentences = [
  'このコーヒーは甘いですが、それでも砂糖を加えます。',
  'タバコは体に悪いと言われている。それでも、止めにくい。',
  '高いのに、それでも買うつもりですか。',
  'それでも行きます。',
];

for (const sentence of testSentences) {
  console.log('\n=== ' + sentence + ' ===');
  const doc = await engine.analyze(sentence);
  if (doc && doc.sentences) {
    for (const sent of doc.sentences) {
      console.log(JSON.stringify(sent.tokens.map(t => ({
        text: t.text,
        lemma: t.lemma,
        pos: t.pos,
        dep: t.dep,
        head: t.head,
      })), null, 2));
    }
  }
}

await engine.close();
