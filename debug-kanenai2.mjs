import { GrammarEngine } from './packages/grammar/src/program.js';

const engine = new GrammarEngine({
  rulesets: [{
    id: 'test',
    rules: [],
  }],
});

await engine.start();

const sentence = '事故は起こりかねない。';
console.log(`\n=== Analyzing: ${sentence} ===\n`);

const ginzaDocs = await engine.ginza.analyze([sentence]);
const doc = ginzaDocs[0];

for (const sent of doc.sentences) {
  for (const tok of sent.tokens) {
    console.log(JSON.stringify({
      id: tok.i,
      text: tok.text,
      lemma: tok.lemma,
      pos: tok.pos,
      tag: tok.tag,
      inflectionForm: tok.inflectionForm,
      conjugationClass: tok.conjugationClass,
      inflection: tok.inflection,
    }, null, 2));
  }
}

await engine.stop();
