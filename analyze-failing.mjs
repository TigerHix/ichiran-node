import { GrammarEngine } from '/home/tiger/ichiran-node/packages/grammar/src/program.js';

const engine = await GrammarEngine.create([]);

const sentence = '寒すぎてスキーウェアを着た。それでも、暖かくならなかった。';
console.log('=== ' + sentence + ' ===');
const doc = await engine.analyze(sentence);
if (doc && doc.sentences) {
  for (const sent of doc.sentences) {
    for (const t of sent.tokens) {
      if (t.text.includes('それ') || t.text.includes('で') || t.text.includes('も')) {
        console.log(`Token: ${t.text}, lemma: ${t.lemma}, pos: ${t.pos}, dep: ${t.dep}, head: ${t.head}`);
      }
    }
    console.log('\nFull parse:');
    console.log(JSON.stringify(sent.tokens.map(t => ({
      text: t.text,
      lemma: t.lemma,
      pos: t.pos,
      dep: t.dep,
      head: t.head,
    })), null, 2));
  }
}

await engine.close();
