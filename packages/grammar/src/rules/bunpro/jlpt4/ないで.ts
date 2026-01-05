import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ないで', (r) => {
  // ないで - Negative te-form: "without doing X, Y happened"
  // This is the conjunctive form of verb negation, used to express
  // that action B happens without doing action A.
  //
  // Examples:
  // - 魚を焼かないで食べた (ate fish without cooking it)
  // - 朝ご飯を食べないで学校に行った (went to school without eating breakfast)
  // - 後悔しないで生きたい (want to live without regret)
  //
  // Structure: Verb[未然形] + ない + で
  // The verb is in mizenkei (irrealis/negative stem) followed by
  // auxiliary ない (negative marker), then で (te-form connective particle).
  //
  // Note: This pattern is structurally identical to:
  // - verb-ないで (casual negative request: "please don't")
  // - ないでください (polite negative request)
  //
  // The distinction is pragmatic/contextual:
  // - "without doing": connects two clauses, action B happens without A
  // - "please don't": request form, typically ends sentence or fragment
  //
  // This rule matches the ないで pattern; context determines usage.

  // The ない auxiliary (negative marker)
  // text: 'ない' - dictionary form (distinguishes from なくて)
  // lemma: 'ない' - the negative auxiliary
  // Note: GiNZA can parse this as either AUX or ADJ depending on context
  const nai = r.tok(
    {
      text: 'ない',
      lemma: 'ない',
      posOneOf: ['AUX', 'ADJ'],
    },
    'nai'
  );

  // The で particle (te-form connective)
  // text: 'で' - distinguishes from て (positive te-form)
  // pos: 'SCONJ' - subordinating conjunction
  // dep: 'mark' - marker for the subordinate clause
  const de = r.tok(
    {
      text: 'で',
      pos: 'SCONJ',
      dep: 'mark',
    },
    'de'
  );

  // で comes immediately after ない (inOrder constraint with distance 1)
  r.inOrder(nai, de, 1);

  // Capture from ない to で (the negative te-form pattern)
  r.captureSpan('ないで', nai, de);
});
