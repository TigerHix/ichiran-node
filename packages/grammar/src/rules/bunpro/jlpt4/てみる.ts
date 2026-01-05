import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てみる (te-form verb + みる) - to try doing something
 *
 * Expresses trying something to see how it goes. "To try doing / To do and see what happens"
 *
 * Examples:
 * - 食べてみる (try eating / eat and see)
 * - 行ってみた (tried going)
 * - してみてください (please try and do)
 * - みたい (want to try)
 * - みます (will try - polite)
 *
 * This is used when doing something for the first time or to see the result.
 * The verb みる is usually written in hiragana (not 見る) for this grammar.
 *
 * GiNZA parsing notes:
 * - Verb te-form: verb stem + て (SCONJ, dep=mark)
 * - みる: verb with lemma=みる (or 見る)
 * - Various conjugations: みる, みた, みて, みたい, みます, みました
 *
 * Forms handled:
 * - Present: てみる
 * - Past: てみた
 * - Te-form: てみて
 * - Polite: てみます, てみました
 * - Desire: てみたい (want to try)
 */
export default bunproLinguisticRule('てみる', (r) => {
  r.either(
    // Pattern 0: Casual command/request (てみ/でみ) - sentence ends with te-form
    // This must come FIRST because it's the most specific (no auxiliaries)
    // e.g., 投げてみ, 食べてみ, してみ
    // GiNZA: verb + て/で + み(lemma=みる, 連用形) - no auxiliary follows
    (b0) => {
      const te = b0.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const mi = b0.tok({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般',
      }, 'mi');

      b0.inOrder(te, mi, 1);
      b0.captureSpan('てみる', te, mi);
    },

    // Pattern 1: Standard present form (てみる/でみる)
    // e.g., 食べてみる, 行ってみる, 飲んでみる, 泳いでみる, してみる
    // GiNZA: verb(連用形) + て/で(SCONJ, dep=mark) + みる(VERB, lemma=みる/見る)
    (b1) => {
      const te = b1.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const miru = b1.verb({
        lemmaOneOf: ['みる', '見る'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'miru');

      b1.inOrder(te, miru, 1);
      b1.captureSpan('てみる', te, miru);
    },

    // Pattern 2: Past form (てみた/でみた)
    // e.g., 食べてみた, 行ってみた, 飲んでみた, してみた
    // GiNZA: verb + て/で + み(lemma=みる, 連用形) + た(AUX)
    (b2) => {
      const te = b2.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const mi = b2.tok({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般',
      }, 'mi');
      const ta = b2.aux({ lemma: 'た', dep: 'aux' }, 'ta');

      b2.inOrder(te, mi, 1);
      b2.inOrder(mi, ta, 1);
      b2.captureSpan('てみる', te, ta);
    },

    // Pattern 3: Te-form (てみて/でみて)
    // e.g., 食べてみて, 行ってみて, 飲んでみて, してみて
    // GiNZA: verb + て/で + み(lemma=みる, 連用形) + て/で(SCONJ)
    (b3) => {
      const te1 = b3.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te1');
      const mi = b3.tok({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般',
      }, 'mi');
      const te2 = b3.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te2');

      b3.inOrder(te1, mi, 1);
      b3.inOrder(mi, te2, 1);
      b3.captureSpan('てみる', te1, te2);
    },

    // Pattern 4: Polite present (てみます/でみます)
    // e.g., 食べてみます, 行ってみます, 飲んでみます, してみます
    // GiNZA: verb + て/で + みます(lemma=みる, 連用形) + ます(AUX)
    (b4) => {
      const te = b4.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const mimasu = b4.tok({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般',
      }, 'mimasu');
      const masu = b4.aux({ lemma: 'ます', dep: 'aux' }, 'masu');

      b4.inOrder(te, mimasu, 2);  // Allow distance for auxiliary
      b4.inOrder(mimasu, masu, 1);
      b4.captureSpan('てみる', te, masu);
    },

    // Pattern 5: Polite past (てみました/でみました)
    // e.g., 食べてみました, 行ってみました, 飲んでみました, してみました
    // GiNZA: verb + て/で + みまし(lemma=みる, 連用形) + た(AUX)
    (b5) => {
      const te = b5.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const mimashita = b5.tok({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般',
      }, 'mimashita');
      const mashita = b5.tok({ pos: 'AUX' }, 'mashita');

      b5.inOrder(te, mimashita, 2);  // Allow distance for auxiliary
      b5.inOrder(mimashita, mashita, 1);
      b5.captureSpan('てみる', te, mashita);
    },

    // Pattern 6: Desire form (てみたい/でみたい)
    // e.g., 食べてみたい, 行ってみたい, 飲んでみたい, してみたい
    // GiNZA: verb + て/で + み(lemma=みる, 連用形) + たい(AUX)
    (b6) => {
      const te = b6.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const mi = b6.tok({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般',
      }, 'mi');
      const tai = b6.aux({ lemma: 'たい', dep: 'aux' }, 'tai');

      b6.inOrder(te, mi, 1);
      b6.inOrder(mi, tai, 1);
      b6.captureSpan('てみる', te, tai);
    },

    // Pattern 7: Desire polite (てみたいです/でみたいです)
    // e.g., 食べてみたいです, 行ってみたいです, 飲んでみたいです, してみたいです
    // GiNZA: verb + て/で + み(lemma=みる, 連用形) + たい(AUX) + です(AUX)
    (b7) => {
      const te = b7.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const mi = b7.tok({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般',
      }, 'mi');
      const tai = b7.aux({ lemma: 'たい', dep: 'aux' }, 'tai');
      const desu = b7.aux({ lemma: 'です', dep: 'aux' }, 'desu');

      b7.inOrder(te, mi, 1);
      b7.inOrder(mi, tai, 1);
      b7.inOrder(tai, desu, 1);
      b7.captureSpan('てみる', te, desu);
    }
  );
});
