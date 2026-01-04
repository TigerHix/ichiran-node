import { analyze } from './src/rules/bunpro/_test/engine.js';

const sentences = [
  '友達として最高だ。',
  '先生としての彼を尊敬します。',
  'これは会社としての目標です。',
  '私たちは先生としての彼を尊敬します。',
  '妻としての役割をよく果たしてくれている。',
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await analyze(s);
  console.log(JSON.stringify(doc.tokens.map(t => ({
    text: t.text,
    lemma: t.lemma,
    pos: t.pos,
    dep: t.dep,
    head: t.head,
    tag: t.tag,
    inflectionForm: t.inflectionForm
  })), null, 2));
}
