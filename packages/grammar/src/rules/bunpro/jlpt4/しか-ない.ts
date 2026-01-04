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
    // Pattern 1: Noun + しか + ない (existential verb or auxiliary)
    // Handles: お茶しかない, 牛乳しかない (where ない is "not exist")
    (b) => {
      const noun = b.noun({
        // Exclude adverbs like ちょっと
        textNoneOf: ['ちょっと', 'すこし', '多少', '幾分'],
      }, 'noun');

      // ない can be either AUX (助動詞-ナイ)
      const nai = b.tok({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');

      // noun + しか + ない (e.g., お茶しかない)
      b.inOrder(noun, shika, 3);
      b.inOrder(shika, nai, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 2: Noun + しか + verb in 未然形-一般 + ない
    // Handles: 牛乳しか残っていない, 人しかいわない, ここにしかすまない
    (b) => {
      const noun = b.noun({
        textNoneOf: ['ちょっと', 'すこし', '多少'],
      }, 'noun');

      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);

      b.inOrder(noun, shika, 3);
      b.inOrder(shika, verb, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 3: Negative polite (～ません)
    // Noun + しか + Verb［連用形］ません
    // ここしかありません (koko + shika + arimasen)
    // ここしかいません (koko + shika + imasen)
    (b) => {
      const noun = b.noun({}, 'noun');

      const verb = b.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');
      b.auxOf(verb, mase);

      b.inOrder(noun, shika, 3);
      b.inOrder(shika, mase, 10);

      b.captureSpan('しか-ない', shika, mase);
    },

    // Pattern 4: Negative semi-polite (～ないです)
    // Noun + しか + Verb［未然形］ないです or Noun + しか + ないです
    (b) => {
      const noun = b.noun({
        textNoneOf: ['ちょっと', 'すこし', '多少'],
      }, 'noun');

      // Optional verb before nai
      b.optional((ob) => {
        const verb = ob.verb({
          inflectionForm: '未然形-一般',
        }, 'verb');
        const nai = ob.aux({
          lemma: 'ない',
          conjugationClass: '助動詞-ナイ',
        }, 'nai');
        ob.auxOf(verb, nai);
        ob.inOrder(noun, verb, 10);
        ob.inOrder(shika, nai, 10);

        const desu = ob.aux({
          lemma: 'です',
        }, 'desu');
        ob.inOrder(nai, desu, 1);
        ob.captureSpan('しか-ない', shika, desu);
      });

      // No verb: Noun + しか + ないです
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      const desu = b.aux({
        lemma: 'です',
      }, 'desu');
      b.inOrder(nai, desu, 1);

      b.inOrder(noun, shika, 3);
      b.inOrder(shika, desu, 10);

      b.captureSpan('しか-ない', shika, desu);
    },

    // Pattern 5: Progressive negative (～ていない)
    // Noun + しか + Verb［連用形］ている in 未然形 + ない
    // ３人しか捕まっていない (sannin + shika + tsukamatte + iru + nai)
    (b) => {
      const noun = b.noun({}, 'noun');

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

      b.inOrder(noun, shika, 3);
      b.inOrder(shika, verb, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 6: Potential negative (～れない / ～できない)
    // Noun + しか + Verb［未然形］ない (potential form)
    // 一匹しか釣れない (ippiki + shika + tsure + nai)
    // ことしかできない (koto + shika + deki + nai)
    (b) => {
      const noun = b.noun({}, 'noun');

      const verb = b.verb({
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);

      b.inOrder(noun, shika, 3);
      b.inOrder(shika, verb, 10);

      b.captureSpan('しか-ない', shika, nai);
    },

    // Pattern 7: Past negative (～なかった)
    // Noun + しか + Verb［連用形］なかった
    // ロウソクしかとらなかった (rousoku + shika + toreta + nakatta)
    (b) => {
      const noun = b.noun({}, 'noun');

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

      b.inOrder(noun, shika, 3);
      b.inOrder(shika, verb, 10);

      b.captureSpan('しか-ない', shika, ta);
    }
  );
});
