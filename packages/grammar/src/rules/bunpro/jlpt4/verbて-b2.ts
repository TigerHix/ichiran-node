import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('verbて-b2', (r) => {
  // Match て-form used for giving reasons or causes
  // Patterns:
  //   Verb[て] + Phrase (e.g., 帰って悲しいです)
  //   [い]Adj[て] + Phrase (e.g., うるさくて勉強できない)
  //   ［な］Adj + で + Phrase (e.g., きれいでのんびりできる)
  //   Noun + で + Phrase (e.g., 病気で休んだ)
  // Meaning: "because", "due to", "since" - causal conjunction
  //
  // This grammatically is similar to verb-て-b (sequence) and verbて-b (contrast),
  // but pragmatically expresses cause/reason instead. The "because" nuance is
  // influenced by the second clause expressing something uncontrollable or a result.

  r.either(
    // Pattern 1: SCONJ て/で as mark (verb te-forms, i-adj te-forms)
    (b1) => {
      // Match any て/で that is a conjunctive particle (mark)
      // This handles verbs and i-adjectives in te-form
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b1.capture(te);
    },
    // Pattern 2: AUX で (na-adjective + だ → で)
    (b2) => {
      // Na-adjectives with copula だ conjunctive form (で)
      // e.g., きれいで, 静かで
      const de = b2.aux({ text: 'で', lemma: 'だ', dep: 'aux' }, 'de');
      b2.capture(de);
    },
    // Pattern 3: Noun/phrase + で with obl/advcl dependency
    // This handles cases like 病気で, 交通事故で
    // These are tricky because で is parsed as ADP with dep=case
    // We need to detect this by context - the noun has dep=obl or dep=advcl
    (b3) => {
      const noun = b3.noun({ depOneOf: ['obl', 'advcl'] }, 'noun');
      const de = b3.tok({ text: 'で', pos: 'ADP', dep: 'case' }, 'de');
      b3.caseMarker(noun, de);
      b3.captureSpan('noun-de', noun, de);
    }
  );
});
