import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';

const engine = useSharedEngine([]).get();

const sentences = [
  '今日運動してよかった。',
  'このレストランにまたきてよかった。',
  'あの携帯を買わなくてよかった。',
  'このバスにまにあってよかったです。',
  'ダイエットをあきらめなくてよかった。',
  'おばあちゃんが亡くなる前にもう一度あえてよかった。',
];

for (const sent of sentences) {
  console.log('\n=== ' + sent + ' ===');
  const doc = await engine.analyze(sent);
  console.log(JSON.stringify(doc, null, 2));
}
