import { analyze } from './packages/grammar/src/engine/ginza.js';

const sentences = [
  'こくはくしたんだけど',
  '買おうと思っているのですが',
  'たべてみたことがあるのですが',
  '顔はいいのだけど',
  '行きたいんだけど',
  '窓が開いているからちょっと寒いんですが',
  '顔はいいんだけど',
  'これほしいんだけど',
  'やせられたらいいんだけど',
  '私は明日釣りに行くのですが',
  '新しいパソコンを買いたいのだけど',
  '誕生日パーティーをやるんだけど',
  'オニズカ先生、質問があるのですが',
  'あしたカフェに行くんだけど',
];

for (const sentence of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('SENTENCE:', sentence);
  console.log('='.repeat(80));

  const doc = await analyze(sentence);

  for (const token of doc.tokens) {
    console.log(
      `${token.text.padEnd(15)} ` +
      `POS=${token.pos.padEnd(6)} ` +
      `lemma=${token.lemma.padEnd(10)} ` +
      `dep=${token.dep.padEnd(6)} ` +
      `head=${token.head}`
    );
  }
}
