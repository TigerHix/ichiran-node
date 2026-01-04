import { GrammarEngine } from './dist/program.js';
import { BUNPRO_JLPT4 } from './dist/rules/bunpro/jlpt4/index.js';

const engine = await GrammarEngine.create([BUNPRO_JLPT4]);

const sentences = [
  '沖縄に一度は行こうと思っています。',
  'スティーブン・ホーキングに一回は会いたかったのに…',
  '少なくともこれぐらいは持って行ってください。',
];

for (const s of sentences) {
  console.log('\n=== ' + s + ' ===');
  const doc = await engine.analyze(s);
  if (!doc || !doc.sentences[0]) {
    console.log('No parse');
    continue;
  }
  const sent = doc.sentences[0];
  for (const [i, t] of sent.tokens.entries()) {
    console.log(`#${i} ${t.text}\tpos=${t.pos}\tlemma=${t.lemma}\ttag=${t.tag}\tdep=${t.dep}\thead=${t.head}`);
  }
}
