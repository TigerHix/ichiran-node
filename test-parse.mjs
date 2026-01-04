import { GinzaEngine } from './packages/grammar/src/engine/ginza/index.js';

const engine = new GinzaEngine();

const sentences = [
  '私に漢字を教えてくれない？',
  'こっちに醤油をとってもらえない？',
  '米をたいておいてくれない？',
  '今日だけ自転車を貸してくれないか？',
  'テレビを点けてもらえないか。',
  'スマホをつかわせてくれないか？',
  '夜遅くに電話しないでくれない？',
  'お茶をいれてくれない？',
  'うちの犬を見たら電話してくれないか？',
  '道を調べてくれない？',
];

for (const sentence of sentences) {
  console.log('\n=== ' + sentence + ' ===');
  const doc = await engine.analyze(sentence);
  console.log(JSON.stringify(doc.tokens.map(t => ({
    text: t.text,
    lemma: t.lemma,
    pos: t.pos,
    dep: t.dep,
    head: t.head
  })), null, 2));
}
