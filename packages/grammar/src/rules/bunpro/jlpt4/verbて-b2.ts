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
    // Pattern 1: Verbs in te-form (all inflection variants)
    (b1) => {
      const verb = b1.verb({ dep: 'advcl' }, 'verb');
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b1.headChild(verb, te, 'mark');
      b1.inOrder(verb, te, 1);
      b1.captureSpan('verb-te', verb, te);
    },
    // Pattern 2: i-adjectives in te-form
    (b2) => {
      const adj = b2.adj({ dep: 'advcl' }, 'adj');
      const te = b2.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b2.headChild(adj, te, 'mark');
      b2.inOrder(adj, te, 1);
      b2.captureSpan('adj-te', adj, te);
    },
    // Pattern 3: na-adjectives + で
    (b3) => {
      const adj = b3.adj({ pos: 'ADJ', dep: 'advcl' }, 'adj');
      const de = b3.tok({ text: 'で', posOneOf: ['SCONJ', 'ADP'], dep: 'mark' }, 'de');
      b3.headChild(adj, de, 'mark');
      b3.inOrder(adj, de, 1);
      b3.captureSpan('adj-de', adj, de);
    },
    // Pattern 4: Nouns + で
    (b4) => {
      const noun = b4.noun({ dep: 'advcl' }, 'noun');
      const de = b4.tok({ text: 'で', posOneOf: ['SCONJ', 'ADP'], dep: 'mark' }, 'de');
      b4.headChild(noun, de, 'mark');
      b4.inOrder(noun, de, 1);
      b4.captureSpan('noun-de', noun, de);
    }
  );
});
