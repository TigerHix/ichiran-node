import { describe, it } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';

describe('Debug: Analyze てよかった patterns', () => {
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
    it(sent, async () => {
      const doc = await engine.analyze(sent);
      console.log('\n=== ' + sent + ' ===');
      console.log(JSON.stringify(doc, null, 2));
    });
  }
});
