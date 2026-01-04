import { useSharedEngine } from '/home/tiger/ichiran-node/packages/grammar/src/rules/bunpro/_test/engine.ts';

async function test() {
  const { getEngine } = useSharedEngine([]);
  const engine = getEngine();
  
  const sentences = [
    'サッカーをするのがへたです。',
    '泳ぐのがへた。',
    '漢字を書くのがへた。',
    '料理するのがへた。',
  ];
  
  for (const sent of sentences) {
    console.log('\n=== ' + sent + ' ===');
    const doc = await engine.analyze(sent);
    for (const tok of doc.tokens) {
      console.log(`${tok.text}\t${tok.pos}\t${tok.lemma}\t${tok.dep}\tinflection=${tok.inflectionForm}`);
    }
  }
}

test();
