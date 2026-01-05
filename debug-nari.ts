import { describe, it } from 'bun:test';
import { useSharedEngine } from './packages/grammar/src/rules/bunpro/_test/engine.js';
import { BUNPRO_JLPT1 } from './packages/grammar/src/rules/bunpro/jlpt1/index.js';

const failingSentences = [
  'ご飯にするなり運動するなり好きにすればいいけど、早く寝なさいよ。',
  '父親は娘に：「孤独を感じていたのなら、俺になり母さんになり電話くれればよかったのに。」',
  '完璧な人はいない。大なり小なり欠点はあるものだ。',
  '上司：「具合が悪いなら、時短勤務にするなり欠席するなりしなさい。」',
  'ヨーロッパなら、フランスなりイタリアなり、料理が美味しいところに行きたい。',
  '友達にアドバイスする：「黙ってばかりいないでよ。喧嘩するなり文句を言うなりしてみたら？」',
  '彼女が彼氏に：「電話なり何なりで、早く連絡しなさいよ。」',
];

describe('debug-nari', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);

  for (const sentence of failingSentences) {
    it(sentence, async () => {
      const e = engine.get();
      const doc = await e.analyze(sentence);
      if (!doc) {
        console.log(`\n=== ${sentence} ===`);
        console.log('No doc returned');
        return;
      }
      console.log(`\n=== ${sentence} ===`);
      for (const sent of doc.sentences) {
        for (let i = 0; i < sent.tokens.length; i++) {
          const t = sent.tokens[i];
          console.log(`[${i}] ${t.text}\tpos=${t.pos}\tlemma=${t.lemma}\tdep=${t.dep}\thead=${t.head}`);
        }
      }
    });
  }
});
