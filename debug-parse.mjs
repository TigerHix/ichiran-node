import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/dist/program.js';

const engine = new GrammarEngine([]);
const sentence = '何を言い出すかとおもったら、みんなの前で俺の悪口を言い出した。';
const doc = await engine.analyze(sentence);
console.log(JSON.stringify(doc.tokens.map(t => ({
  text: t.text,
  lemma: t.lemma,
  pos: t.pos,
  dep: t.dep,
  inflectionForm: t.inflectionForm
})), null, 2));
