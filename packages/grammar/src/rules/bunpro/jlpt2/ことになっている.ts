import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことになっている - "it is arranged that, it has been decided that, the rule is"
 *
 * Matches: verb-dictionary form + こと + になっている (ongoing arrangement/state)
 *
 * This expresses that something has been arranged, decided, or is expected to happen,
 * typically by someone other than the speaker. It indicates a rule, schedule, or
 * established arrangement that is currently in effect.
 *
 * Structure variants:
 * - Verb［る］+ ことになっている (casual, present progressive)
 * - Verb［る］+ ことになっています (polite, present progressive)
 * - Verb［る］+ ことになっていた (casual, past progressive)
 * - Verb［る］+ ことになっていました (polite, past progressive)
 * - Verb［る］+ ことになってる (casual, contracted)
 *
 * Contrast with ことになる (JLPT3) which focuses on the decision/outcome itself,
 * while ことになっている focuses on the ongoing state of being arranged/expected.
 *
 * GiNZA parse structure (for "始まる" → "始まることになっている"):
 * - 始まる(NOUN/VERB) --compound--> こと(NOUN)
 * - こと --fixed--> に(ADP)
 * - こと --fixed--> なっ(VERB)
 * - なっ --aux--> いる(AUX) indicating progressive state
 *
 * Key insight: This rule builds on ことになる (JLPT3) but adds ている/ています
 * to indicate the ongoing state or arrangement.
 */
export default linguisticRule('ことになっている', (r) => {
  r.either(
    // Branch 1: Casual present progressive (〜ことになっている)
    // GiNZA parses て+いる as: て(SCONJ,dep=mark) + い(VERB,lemma=いる,dep=fixed)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.headChild(pred, te, 'mark');

      // GiNZA tokenizes いる as 'い' with lemma='いる'
      const iru = b.tok({
        lemma: 'いる',
        inflectionForm: '連用形-一般',
        dep: 'fixed',
      }, 'iru');
      b.inOrder(te, iru, 1);

      b.captureSpan('ことになっている', pred, iru);
    },

    // Branch 1b: Full いる token (not contracted to 'い')
    // Some parses have 'いる' with inflectionForm=終止形-一般
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.headChild(pred, te, 'mark');

      // Full いる token at end of sentence
      const iru = b.tok({
        lemma: 'いる',
        inflectionForm: '終止形-一般',
        dep: 'fixed',
      }, 'iru');
      b.inOrder(te, iru, 1);

      b.captureSpan('ことになっている', pred, iru);
    },

    // Branch 2: Polite present progressive (〜ことになっています)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.headChild(pred, te, 'mark');

      const imasu = b.aux({ lemma: 'ます' }, 'imasu');
      b.inOrder(te, imasu, 2);

      b.captureSpan('ことになっている', pred, imasu);
    },

    // Branch 3: Casual past progressive (〜ことになっていた)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.inOrder(nat, te, 1);

      // い (from いる) + た (past)
      const i = b.tok({
        lemma: 'いる',
        inflectionForm: '連用形-一般',
      }, 'i');
      b.inOrder(te, i, 1);

      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.inOrder(i, ta, 1);

      b.captureSpan('ことになっている', pred, ta);
    },

    // Branch 4: Polite past progressive (〜ことになっていました)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.inOrder(nat, te, 1);

      // いまし (from います) + た
      const imashi = b.tok({
        lemma: 'ます',
        inflectionForm: '連用形-一般',
      }, 'imashi');
      b.inOrder(te, imashi, 1);

      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.inOrder(imashi, ta, 1);

      b.captureSpan('ことになっている', pred, ta);
    }
  );
});
