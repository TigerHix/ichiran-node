import { GiNZA } from './packages/grammar/src/ginza/client.js';

const ginza = new GiNZA();

const testSentences = [
  '事故は起こりかねない。',
  '失敗しかねない。',
  'なりかねない。',
];

for (const sentence of testSentences) {
  console.log(`\n=== ${sentence} ===`);
  const tokens = await ginza.tokenize(sentence);
  for (const tok of tokens) {
    console.log(JSON.stringify({
      text: tok.text,
      lemma: tok.lemma,
      pos: tok.pos,
      inflectionForm: tok.inflectionForm,
      tag: tok.tag,
    }));
  }
}
