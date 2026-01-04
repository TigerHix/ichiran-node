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
 * - Verb［る］+ ことになっています (polite, present progressive)
 * - Verb［る］+ ことになっていました (polite, past progressive)
 * - Verb［る］+ ことになっている (casual, present progressive)
 * - Verb［る］+ ことになっていた (casual, past progressive)
 * - Verb［る］+ ことになってる (casual, contracted)
 *
 * Contrast with ことになる (JLPT3) which focuses on the decision/outcome itself,
 * while ことになっている focuses on the ongoing state of being arranged/expected.
 *
 * GiNZA parse structure (for "始まる" → "始まることになっています"):
 * - 始まる(NOUN/VERB) --compound--> こと(NOUN)
 * - こと --fixed--> に(ADP)
 * - なっ(VERB) --compound--> こと
 * - て --mark--> 始まる (te-form connects to original verb)
 * - い(fixed) --headChild--> て (stem of います)
 * - ます(aux) --headChild--> い (polite ending)
 *
 * Key insight: います can be a single token or decomposed as い+ます.
 * The key is to use `inOrder` to ensure correct ordering without over-constraining
 * the dependency relationships.
 */
export default linguisticRule('ことになっている', (r) => {
  r.either(
    // Branch 1: Polite forms (with ます)
    // This catches both ことになっています and ことになっていました
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

      // い (stem of います/いた)
      const i = b.tok({
        text: 'い',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
        dep: 'fixed',
      }, 'i');
      b.headChild(te, i, 'fixed');

      // Must have ます (polite marker) - could be ます or まし
      b.either(
        // 1a: Present polite: い + ます
        (b2) => {
          const masu = b2.aux({ lemma: 'ます' }, 'masu');
          b2.inOrder(i, masu, 1);
          b2.captureSpan('ことになっている', pred, masu);
        },
        // 1b: Past polite: い + まし + た
        (b2) => {
          const mash = b2.tok({
            text: 'まし',
            lemma: 'ます',
            posOneOf: ['VERB', 'AUX'],
            inflectionForm: '連用形-一般',
          }, 'mash');
          b2.inOrder(i, mash, 1);

          const ta = b2.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
          b2.auxOf(pred, ta);

          b2.captureSpan('ことになっている', pred, ta);
        }
      );
    },

    // Branch 2: Casual present progressive (〜ことになっている)
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

      // いる (casual progressive) - full form "いる" not just "い"
      const iru = b.tok({
        text: 'いる',
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '終止形-一般',
        dep: 'fixed',
      }, 'iru');
      b.headChild(te, iru, 'fixed');

      b.captureSpan('ことになっている', pred, iru);
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
      b.headChild(pred, te, 'mark');

      // い (stem of いる for past tense)
      const i = b.tok({
        text: 'い',
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
        dep: 'fixed',
      }, 'i');
      b.headChild(te, i, 'fixed');

      const ta = b.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
      b.auxOf(pred, ta);

      b.captureSpan('ことになっている', pred, ta);
    },

    // Branch 4: Casual contracted (〜ことになってる)
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

      // る (contracted form of いる in てる)
      const ru = b.tok({
        text: 'る',
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '終止形-一般',
        dep: 'fixed',
      }, 'ru');
      b.headChild(te, ru, 'fixed');

      b.captureSpan('ことになっている', pred, ru);
    }
  );
});
