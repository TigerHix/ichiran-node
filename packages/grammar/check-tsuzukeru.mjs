import { describeRule } from './dist/rules/bunpro/_test/helpers.js';
import { createTestEngine } from './dist/rules/bunpro/_test/engine.js';

async function main() {
  const engine = await createTestEngine([]);
  
  const sentences = [
    '餌をあげても、その犬はなきつづけるよ。',
    '文法の勉強を毎日しつづけたら、すぐ上手になるだろう。',
  ];
  
  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    console.log(JSON.stringify(doc.tokens.map(t => ({
      text: t.text,
      lemma: t.lemma,
      pos: t.pos,
      dep: t.dep,
      inflectionForm: t.inflectionForm,
    })), null, 2));
  }
}

main().catch(console.error);
