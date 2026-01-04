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
    (b) => {
      // Preceding predicate (verb in dictionary form)
      const pred = b.tok({}, 'pred');

      // Followed by こと (nominalizer) - dep=compound points to pred
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      // Followed by に (case marker, fixed)
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // Followed by なっ (連用形-促音便)
      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      // Followed by て (te-form)
      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.inOrder(nat, te, 1);

      // Followed by いる (progressive) - attaches to pred as aux
      const iru = b.aux({ lemma: 'いる', inflectionForm: '終止形-一般' }, 'iru');
      b.auxOf(pred, iru);

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
      b.inOrder(nat, te, 1);

      // います (polite progressive) - attaches to pred as aux
      const imasu = b.aux({ lemma: 'います', inflectionForm: '終止形-一般' }, 'imasu');
      b.auxOf(pred, imasu);

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

      // いた (past progressive) - attaches to pred as aux
      const ita = b.aux({ lemma: 'いる', inflectionForm: '連用形-一般' }, 'ita');
      b.auxOf(pred, ita);

      // た (past) - attaches to pred as aux
      const ta = b.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
      b.auxOf(pred, ta);

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

      // いました (polite past progressive) - can be:
      // - imashita as single aux token
      // - imashita decomposed: いまし(aux of pred) + た(aux of pred)
      b.either(
        // 4a: Single いました token
        (b2) => {
          const imashita = b2.aux({ lemma: 'いました' }, 'imashita');
          b2.auxOf(pred, imashita);
          b2.captureSpan('ことになっている', pred, imashita);
        },
        // 4b: いまし + た decomposed
        (b2) => {
          const imashi = b2.aux({ lemma: 'います', inflectionForm: '連用形-一般' }, 'imashi');
          b2.auxOf(pred, imashi);

          const ta = b2.aux({ lemma: 'た', conjugationClass: '助動詞-タ' }, 'ta');
          b2.auxOf(pred, ta);

          b2.captureSpan('ことになっている', pred, ta);
        }
      );
    },
    // Branch 5: Casual contracted (〜ことになってる)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const nat = b.verb({ lemma: 'なる', dep: 'fixed', inflectionForm: '連用形-促音便' }, 'nat');
      b.inOrder(ni, nat, 1);

      // て (te-form) - may have text=て or be contracted with いる
      const te = b.tok({ lemma: 'て', depOneOf: ['mark', 'fixed'] }, 'te');
      b.inOrder(nat, te, 1);

      // る (contracted form of いる) - attaches to pred as aux
      const ru = b.aux({ lemma: 'いる', text: 'る', inflectionForm: '終止形-一般' }, 'ru');
      b.auxOf(pred, ru);

      b.captureSpan('ことになっている', pred, ru);
    }
  );
});
