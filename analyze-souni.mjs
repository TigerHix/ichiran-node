import { describeRule } from './packages/grammar/src/rules/bunpro/_test/helpers.js';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const { BUNPRO_JLPT4 } = await import('./packages/grammar/src/rules/bunpro/jlpt4/index.js');
const engine = useSharedEngine([BUNPRO_JLPT4]);

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
