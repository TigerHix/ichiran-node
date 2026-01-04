import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('しか-ない', (r) => {
  // しか + negative form (only, nothing but)
  // Pattern: Noun + しか + negative verb/aux
  // - 牛乳しかない (only milk)
  // - ここしかありません (only here - polite)
  // - お茶しかないけど (only tea - casual)

  // The particle しか (shika - adverbial particle expressing exclusivity)
  const shika = r.particle('しか', 'shika');

  r.either(
    // Pattern 1: Negative verb (～ない)
    // Noun + しか + Verb［ない］
    // お茶しかない (ocha + shika + nai)
    // 牛乳しか残っていない (gyuunyuu + shika + nokotte + iru in 未然形-一般 + nai)
    (b) => {
      const noun = b.noun({
        tagOneOf: ['名詞-普通名詞-一般', '名詞-普通名詞-サ変形状', '名詞-普通名詞-サ変可能', '名詞-代名詞'],
      }, 'noun');

      // Negative auxiliary ない
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');

      // Optional: main verb before nai (e.g., 残っていない)
      b.optional((ob) => {
        const verb = ob.verb({
          inflectionForm: '未然形-一般',
        }, 'verb');
        ob.auxOf(verb, nai);
        ob.inOrder(noun, verb, 10);
      });

      // If no verb, noun directly precedes nai (e.g., お茶しかない)
      b.inOrder(noun, nai, 10);
      b.inOrder(shika, nai, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 2: Negative polite (～ません / ～ありません)
    // Noun + しか + ません / ありません
    // ここしかありません (koko + shika + arimasen)
    // ここしかいません (koko + shika + imasen)
    (b) => {
      const noun = b.noun({
        tagOneOf: ['名詞-普通名詞-一般', '名詞-代名詞'],
      }, 'noun');

      // Verb in 連用形-一般 + aux ません
      const verb = b.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');
      b.auxOf(verb, mase);

      b.inOrder(noun, verb, 10);
      b.inOrder(shika, mase, 10);

      b.captureSpan('しか-ない', shika, mase);
    },

    // Pattern 3: Negative semi-polite (～ないです)
    // Noun + しか + ないです
    // ここしかないです (koko + shika + nai + desu)
    (b) => {
      const noun = b.noun({
        tagOneOf: ['名詞-普通名詞-一般', '名詞-代名詞'],
      }, 'noun');

      // Optional verb before nai
      b.optional((ob) => {
        const verb = ob.verb({
          inflectionForm: '未然形-一般',
        }, 'verb');
        ob.inOrder(noun, verb, 10);
      });

      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      const desu = b.aux({
        lemma: 'です',
      }, 'desu');
      b.inOrder(nai, desu, 1);

      b.inOrder(noun, desu, 10);
      b.inOrder(shika, desu, 10);

      b.captureSpan('しか-ない', shika, desu);
    },

    // Pattern 4: Progressive negative (～ていない)
    // Noun + しか + ～ていない
    // ３人しか捕まっていない (sannin + shika + tsukamatte + iru in 未然形-一般 + nai)
    (b) => {
      const noun = b.noun({
        tagOneOf: ['名詞-普通名詞-一般', '名詞-数詞', '名詞-代名詞'],
      }, 'noun');

      const verb = b.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const te = b.tok({
        pos: 'SCONJ',
        lemma: 'て',
      }, 'te');
      const iru = b.verb({
        lemma: 'いる',
        inflectionForm: '未然形-一般',
        dep: 'fixed',
      }, 'iru');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');

      b.inOrder(verb, te, 1);
      b.inOrder(te, iru, 1);
      b.auxOf(iru, nai);

      b.inOrder(noun, verb, 10);
      b.inOrder(shika, nai, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 5: Potential negative (～れない / ～できない)
    // Noun + しか + ～れない
    // 一匹しか釣れない (ippiki + shika + tsure + nai - potential form)
    // ことしかできない (koto + shika + deki + nai - potential)
    (b) => {
      const noun = b.noun({
        tagOneOf: ['名詞-普通名詞-一般', '名詞-数詞', '名詞-普通名詞-サ変形状'],
      }, 'noun');

      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);

      b.inOrder(noun, verb, 10);
      b.inOrder(shika, nai, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 6: Past negative (～なかった)
    // Noun + しか + ～なかった
    // ロウソクしかとらなかった (rousoku + shika + totra + nakatta)
    (b) => {
      const noun = b.noun({
        tagOneOf: ['名詞-普通名詞-一般', '名詞-代名詞'],
      }, 'noun');

      const verb = b.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const nakat = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
        inflectionForm: '連用形-促音便',
      }, 'nakat');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');
      b.inOrder(verb, nakat, 1);
      b.inOrder(nakat, ta, 1);

      b.inOrder(noun, verb, 10);
      b.inOrder(shika, ta, 10);

      b.captureSpan('しか-ない', shika, ta);
    }
  );
});
