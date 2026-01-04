import { analyze } from '/home/tiger/ichiran-node/packages/grammar/src/rules/bunpro/_test/engine.js';

const sentences = [
  '一ヶ月おきに病院に来るように先生に言われました。',
  'この薬は一日おきに飲んでください。',
  '二日おきに部屋の掃除をしないと気が落ち着かない。',
  '一日ごとに目薬を差す。',
  '３０分おきに休憩しよう。',
  '１時間おきに温度を測ってください。',
];

for (const sentence of sentences) {
  console.log('\n' + '='.repeat(80));
  console.log('Sentence:', sentence);
  console.log('='.repeat(80));
  const doc = await analyze(sentence);

  for (const token of doc.tokens) {
    console.log(JSON.stringify({
      text: token.text,
      lemma: token.lemma,
      pos: token.pos,
      tag: token.tag,
      dep: token.dep,
      head: token.head,
      inflectionForm: token.inflectionForm,
    }));
  }
}
