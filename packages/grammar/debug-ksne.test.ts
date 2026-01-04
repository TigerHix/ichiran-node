import { describe, it, expect } from 'bun:test';
import { useSharedEngine } from './src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT3 } from './src/rules/bunpro/index.js';

const failingSentences = [
  '見てたくせに、なんで「見てない」って嘘をつくの？',
  'さっき寝るって言ってたくせにまだ起きてるの？',
  '若いくせに何ダラダラしているんだ。',
  '自分だって下手なくせに何偉そうに言ってるんだ。',
  'この子は犬のくせにニャーと鳴く。',
  '休みで時間があるくせに、家の手伝いを何もしない。',
  '暑がりなくせに、あの人は毎日セーターを着て仕事に行く。',
  '食べきれないくせにたくさん注文するのはやめてほしい。',
  '何も知らないくせに、偉そうなことを言うのはやめてください。',
  'お金もないくせに、カードで気軽に高い物を買うのはやめた方がいいよ。',
  '本当は行きたいくせに。素直に「行きたい」って言えばいいじゃないか。',
  '寒いくせに「寒くない」とやせ我慢するのはやめてください。',
  '「男のくせに泣くな」って言われるけど、たまには泣いていいと思う。',
  'あの人は先輩のくせに、いつも僕におごらせようとする。',
  '昨日休んだくせに、なんで知ったかぶりするの？',
  '給料を沢山もらっているくせに、貯金がないらしい。'
];

console.log('Analyzing failing sentences for くせに rule:\n');

describe('Debug くせに', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);

  for (const sentence of failingSentences.slice(0, 1)) {
    it(`Analyze: ${sentence}`, async () => {
      const eng = engine.get();
      const result = await eng.analyze(sentence);

      console.log(`\n=== ${sentence} ===`);
      console.log('Result:', JSON.stringify(result, null, 2));

      // Try to explain the match
      const explain = await eng.explainMatch(sentence, 'くせに');
      console.log('Explain result:', explain);
    });
  }
});