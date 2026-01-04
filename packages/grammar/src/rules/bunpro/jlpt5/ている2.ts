import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: ている2 (resultative state)
 *
 * Resultative state: Verb[て] + いる (state resulting from an action)
 * Different from ている1 - this shows a current state, not ongoing action
 *
 * Examples:
 * - 結婚している (is married - state resulting from getting married)
 * - 知っている (knows - state resulting from learning)
 * - 死んでいる (is dead - state resulting from dying)
 * - 始まっている (has started - state resulting from starting)
 *
 * Grammar structure:
 * - Verb in te-form (連用形 + て/で SCONJ)
 * - いる auxiliary verb (VERB with lemma=いる, dep=fixed)
 * - Optional: ます (AUX) for polite form
 * - Optional: ない (AUX) for negative
 * - Optional: てる (AUX, lemma=てる) for casual contraction
 *
 * GiNZA parses this as:
 * - ている: VERB(連用形) + SCONJ(て, dep=mark) + VERB(いる, dep=fixed)
 * - ています: VERB(連用形) + SCONJ(て, dep=mark) + VERB(いる, dep=fixed, 連用形) + AUX(ます, dep=aux)
 * - ていない: VERB(連用形) + SCONJ(て, dep=mark) + VERB(いる, dep=fixed, 未然形) + AUX(ない, dep=aux)
 * - てる: VERB(連用形) + AUX(てる, dep=aux) - single token!
 * - でいる: Same as ている but with で for verbs like 死ぬ
 */
export default linguisticRule('ている2', (r) => {
  r.either(
    // Pattern 1: Standard form (ている) - normal verbs
    // e.g., 知っている, 始まっている, 怒っている
    (b1) => {
      const verb = b1.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b1.verb({
        lemma: 'いる',
        dep: 'fixed',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般']
      }, 'iru');

      b1.headChild(verb, te, 'mark');
      b1.inOrder(verb, te, 1);
      b1.inOrder(te, iru, 1);
      b1.captureSpan('ている2', verb, iru);
    },

    // Pattern 1b: Standard form (ている) - suru-verbs (noun+する)
    // e.g., 結婚している, 勉強している
    // GiNZA parses suru-verbs differently: the te-form particle attaches to the main verb (noun),
    // not to the suru auxiliary
    (b1b) => {
      const verb = b1b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b1b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b1b.verb({
        lemma: 'いる',
        dep: 'fixed',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般']
      }, 'iru');

      b1b.inOrder(verb, te, 1);
      b1b.inOrder(te, iru, 1);
      b1b.captureSpan('ている2', verb, iru);
    },

    // Pattern 2: Polite form (ています) - normal verbs
    // e.g., 知っています, 始まっています, 怒っています
    (b2) => {
      const verb = b2.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b2.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b2.verb({ lemma: 'いる', dep: 'fixed', inflectionForm: '連用形-一般' }, 'iru');
      const masu = b2.aux({ lemma: 'ます', dep: 'aux' }, 'masu');

      b2.headChild(verb, te, 'mark');
      b2.inOrder(verb, te, 1);
      b2.inOrder(te, iru, 1);
      b2.inOrder(iru, masu, 1);
      b2.auxOf(verb, masu);
      b2.captureSpan('ている2', verb, masu);
    },

    // Pattern 2b: Polite form (ています) - suru-verbs
    (b2b) => {
      const verb = b2b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b2b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b2b.verb({ lemma: 'いる', dep: 'fixed', inflectionForm: '連用形-一般' }, 'iru');
      const masu = b2b.aux({ lemma: 'ます', dep: 'aux' }, 'masu');

      b2b.inOrder(verb, te, 1);
      b2b.inOrder(te, iru, 1);
      b2b.inOrder(iru, masu, 1);
      b2b.captureSpan('ている2', verb, masu);
    },

    // Pattern 3: Negative form (ていない) - normal verbs
    // e.g., 知っていない, 結婚していない
    (b3) => {
      const verb = b3.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b3.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b3.verb({ lemma: 'いる', dep: 'fixed', inflectionForm: '未然形-一般' }, 'iru');
      const nai = b3.aux({ lemma: 'ない', dep: 'aux' }, 'nai');

      b3.headChild(verb, te, 'mark');
      b3.inOrder(verb, te, 1);
      b3.inOrder(te, iru, 1);
      b3.inOrder(iru, nai, 1);
      b3.auxOf(verb, nai);
      b3.captureSpan('ている2', verb, nai);
    },

    // Pattern 3b: Negative form (ていない) - suru-verbs
    (b3b) => {
      const verb = b3b.aux({
        lemma: 'する',
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const te = b3b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b3b.verb({ lemma: 'いる', dep: 'fixed', inflectionForm: '未然形-一般' }, 'iru');
      const nai = b3b.aux({ lemma: 'ない', dep: 'aux' }, 'nai');

      b3b.inOrder(verb, te, 1);
      b3b.inOrder(te, iru, 1);
      b3b.inOrder(iru, nai, 1);
      b3b.captureSpan('ている2', verb, nai);
    },

    // Pattern 4: Casual contraction (てる)
    // e.g., 知ってる, 死んでる
    // GiNZA parses てる as a single AUX token with lemma=てる
    (b4) => {
      const verb = b4.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
        ]
      }, 'verb');
      const teru = b4.aux({ lemma: 'てる', dep: 'aux' }, 'teru');

      b4.auxOf(verb, teru);
      b4.captureSpan('ている2', verb, teru);
    },

    // Pattern 5: Casual contraction (でる)
    // e.g., 死んでる - GiNZA sometimes parses this as でる
    (b5) => {
      const verb = b5.verb({
        inflectionFormOneOf: [
          '連用形-一般',
          '連用形-イ音便',
          '連用形-撥音便',
          '連用形-促音便',
          '連用形-ウ音便',
          '未然形-撥音便',
        ]
      }, 'verb');
      const deru = b5.aux({ lemma: 'でる', dep: 'aux' }, 'deru');

      b5.auxOf(verb, deru);
      b5.captureSpan('ている2', verb, deru);
    }
  );
});
