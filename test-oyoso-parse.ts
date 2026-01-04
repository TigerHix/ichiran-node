import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

async function main() {
  const engine = useSharedEngine([]);

  const testSentences = [
    'この図書館にはおよそ一万冊の本があるらしい。',
    'カーナビ：「目的地までおおよそ３０分です。」',
    'パーティーにくるおおよその人数を把握する。',
    '美術館では、凡そ高価な作品を展示している。',
    '地震による犠牲者はおおよそ１２０人です。',
    '事情はおおよそ見当がつくが、一応説明してくれるかい？',
  ];

  for (const sentence of testSentences) {
    console.log('\n=== Sentence:', sentence, '===');
    const doc = await engine.get().analyze(sentence);

    for (const token of doc.tokens) {
      if (token.lemma && (token.lemma.includes('およそ') || token.lemma.includes('おおよそ') || token.text.includes('およそ') || token.text.includes('おおよそ'))) {
        console.log(`Text: ${token.text}`);
        console.log(`  Lemma: ${token.lemma}`);
        console.log(`  POS: ${token.pos}`);
        console.log(`  Dep: ${token.dep}`);
        console.log(`  Head: ${token.head}`);
      }
    }
  }
}

main();
