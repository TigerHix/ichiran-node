import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('と-with', (r) => {
  // Match と as "with" particle (accompaniment)
  // Distinct from:
  // - Conditional と (SCONJ + mark dep) - different POS
  // - Quotation と (ADP + case dep, but head is quoted clause with ccomp/advcl/acl/discourse dep)
  // - "And" listing と (ADP + case dep, structurally identical but semantically different)
  //
  // Strategy: Match と that marks a noun (not a quoted clause)
  // - と has pos=ADP, dep=case
  // - Noun marked by と has dep=obl or dep=nmod
  // - The head of と should be the noun it marks (not a clause)
  //
  // Note: "With" vs "and" are structurally identical in Japanese.
  // Both use と to mark nouns with dep=obl or dep=nmod.
  // This rule will match both structures, which is appropriate since
  // the distinction is semantic/pragmatic rather than syntactic.

  const to = r.tok({
    text: 'と',
    pos: 'ADP',
    dep: 'case'
  }, 'to');

  const noun = r.tok({
    posOneOf: ['NOUN', 'PRON', 'PROPN'],
    depOneOf: ['obl', 'nmod']
  }, 'noun');

  // Require と to mark a noun (case marking relationship)
  r.caseMarker(noun, to);

  // Capture the と particle
  r.capture(to);
});
