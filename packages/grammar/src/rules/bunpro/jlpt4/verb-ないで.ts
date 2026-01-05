import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('verb-ないで', (r) => {
  // Casual negative request: verb nai-form + で (please don't)
  // Examples: 行かないで (please don't go), 食べないで (please don't eat)
  //
  // Structure: Verb[未然形] + ない + で
  // The verb is in mizenkei (irrealis form) followed by auxiliary ない
  // Then で (te-form connector in negative context)
  //
  // This pattern is used for casual negative requests.
  // It typically ends the sentence or sentence fragment.
  //
  // Note: This pattern is structurally identical to:
  // - ないでください (polite form) - has ください after ないで
  // - ないで ("without doing") - connects two clauses
  //
  // The distinction is pragmatic/contextual, not structural.
  // This rule matches the ないで pattern; context determines the usage.

  // The ない auxiliary (negative marker)
  const nai = r.aux(
    {
      text: 'ない',
      lemma: 'ない',
    },
    'nai'
  );

  // The で particle (connective form in negative context)
  const de = r.tok(
    {
      text: 'で',
      lemma: 'で',
      pos: 'SCONJ',
      dep: 'mark',
    },
    'de'
  );

  // で comes after ない (inOrder constraint)
  r.inOrder(nai, de, 1);

  // Capture from ない to で (the casual negative request pattern)
  r.captureSpan('verb-ないで', nai, de);
});
