import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const engine = useSharedEngine([]).get();

async function main() {
  const testSentences = [
    'もし失敗したら失敗したで、また最初からやり直そうと思う。',
    '結婚したい。結婚したらしたで大変だよ。',
    '子供になったらなったで子供なりの苦労があるよ',
  ];

  for (const sentence of testSentences) {
    console.log('\n========================================');
    console.log('Sentence:', sentence);
    console.log('========================================');
    const doc = await engine.analyze(sentence);
    console.log(JSON.stringify(doc, null, 2));
  }
}

await main();
