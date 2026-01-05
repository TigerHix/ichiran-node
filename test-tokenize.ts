import { GrammarEngine, BUNPRO_RULESETS } from './packages/grammar/src/index.js';

async function main() {
  const grammarEngine = await GrammarEngine.create(BUNPRO_RULESETS);

  const text = '昨日、友達と映画を見に行きました';
  console.log('Testing:', text);
  console.log();

  const doc = await grammarEngine.analyze(text);
  if (!doc) {
    console.log('No doc found');
    return;
  }

  console.log('Tokens:');
  for (const sent of doc.sentences) {
    for (const token of sent.tokens) {
      console.log(JSON.stringify({
        id: token.i,
        text: token.text,
        lemma: token.lemma,
        pos: token.pos,
        inflectionForm: token.inflectionForm,
        conjugationClass: token.conjugationClass,
        head: token.head,
        dep: token.dep,
        inflection: token.inflection,
      }, null, 2));
    }
  }

  await grammarEngine.close();
}

main().catch(console.error);
