import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.ts';

const engine = useSharedEngine([]);

// Test sentences from the JSON
const testSentences = [
  '優しそうに犬を撫でた。',
  '楽しそうな授業をしています。',
  '眠そうな目をしているんだね。',
  'さっき、先輩が怒りそうになってた。',
  '彼は忙しそうに仕事をしている。',
  'つまらなそうな話は聞きたくない。',
  'これが入りそうな箱ってある？',
  '今日は雨が降りそうな日ですね。',
];

for (const sent of testSentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await engine.get().analyze(sent);
  console.log(JSON.stringify(doc, null, 2));
}
