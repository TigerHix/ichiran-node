import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('しか-ない', (r) => {
  // しか + negative form (only, nothing but)
  // Pattern: Noun + しか + negative verb/aux
  // - 牛乳しかない (only milk)
  // - ここしかありません (only here - polite)
  // - お茶しかないけど (only tea - casual)

  // The particle しか (shika - adverbial particle expressing exclusivity)
  const shika = r.particle('しか', 'shika');

  r.either(
    // Pattern 1a: Noun + しか + Verb［ない］ with verb
    // お茶しか残っていない (ocha + shika + nokotte + iru in 未然形-一般 + nai)
    // 「はい」と「いいえ」しかいわない (quoted phrase + shika + verb + nai)
    (b) => {
      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');

      const nai = b.tok({
        lemma: 'ない',
      }, 'nai');
      b.auxOf(verb, nai);

      b.inOrder(shika, verb, 5);
      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 1b: Noun + しか + ない (without verb)
    // お茶しかない (ocha + shika + nai)
    (b) => {
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON', 'ADV', 'NUM', 'PROPN'],
      }, 'noun');

      const nai = b.tok({
        lemma: 'ない',
      }, 'nai');

      b.inOrder(noun, nai, 10);
      b.inOrder(shika, nai, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 2: Negative polite (～ません / ～ありません)
    // Noun + しか + ません / ありません
    // ここしかありません (koko + shika + arimasen)
    // ここしかいません (koko + shika + imasen)
    (b) => {
      // Include various POS types
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON', 'ADV', 'NUM', 'PROPN'],
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
      // Include various POS types
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON', 'ADV', 'NUM', 'PROPN'],
      }, 'noun');

      // Optional verb before nai
      b.optional((ob) => {
        const verb = ob.verb({
          inflectionForm: '未然形-一般',
        }, 'verb');
        ob.inOrder(noun, verb, 10);
      });

      const nai = b.tok({
        lemma: 'ない',
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
      // Include various POS types
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON', 'ADV', 'NUM', 'PROPN'],
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
      const nai = b.tok({
        lemma: 'ない',
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
      // Include various POS types
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON', 'ADV', 'NUM', 'PROPN'],
      }, 'noun');

      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.tok({
        lemma: 'ない',
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
      // Include various POS types
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON', 'ADV', 'NUM', 'PROPN'],
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
