import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('くらい2', (r) => {
  // くらい/ぐらい expressing "to the extent that" or "so...that"
  // Follows: Verb, [い]Adj, [な]Adj+な, or Noun
  // This is the extent/degree usage (JLPT3), distinct from approximate amount (JLPT5)
  //
  // GiNZA parses this particle as:
  // - ADP/PART with dep='case' or dep='mark'
  // - Always immediately follows the predicate/phrase it modifies

  r.either(
    // Pattern 1: Any word (NOUN/VERB/AUX/ADJ/DET/PROPN) + くらい/ぐらい
    // This captures the extent/degree usage where X+くらい modifies what follows
    (b) => {
      const predicate = b.tok({ posOneOf: ['NOUN', 'VERB', 'AUX', 'ADJ', 'PROPN', 'DET', 'PRON'] }, 'predicate');
      const kurai = b.tok({
        textOneOf: ['くらい', 'ぐらい'],
        posOneOf: ['ADP', 'PART'],
        depOneOf: ['case', 'mark']
      }, 'kurai');
      b.inOrder(predicate, kurai, 1);
      b.captureSpan('くらい', predicate, kurai);
    }
  );
});
