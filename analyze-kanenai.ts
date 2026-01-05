import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const engine = useSharedEngine([]).get();

const sentences = [
  'この書類を開示されてはスキャンダルがおきかねないから、処分する必要があります。',
  '灯油の保管をちゃんとしないと、火事になりかねない。',
  '彼は問題をふやしかねないから、誰かちゃんとアドバイスしてください。',
  '危険な運転をすれば、事故がおこりかねない。',
  'あの様子だとまた家出しかねない。そうならないように、話を聞いてあげてください。',
  'この犬に噛まれたら、大怪我をしかねない。',
];

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===\n');
  const doc = await engine.analyze(sent);

  // Focus on tokens around かねない
  for (let i = 0; i < doc.tokens.length; i++) {
    const tok = doc.tokens[i];
    if (tok.lemma?.includes('かね') || tok.text?.includes('かね') ||
        tok.lemma?.includes('兼') || tok.text?.includes('兼')) {
      // Show context around this token
      console.log('Token ' + i + ':');
      console.log(JSON.stringify(tok, null, 2));
      if (i > 0) {
        console.log('  Prev (' + (i-1) + '): ' + JSON.stringify(doc.tokens[i-1]));
      }
      if (i < doc.tokens.length - 1) {
        console.log('  Next (' + (i+1) + '): ' + JSON.stringify(doc.tokens[i+1]));
      }
      console.log('');
    }
  }
}
