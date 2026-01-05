import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('きり', (r) => {
  // きり meaning "only/just/since"
  // Patterns:
  // 1. Verb (past form) + きり/っきり - "since doing X, haven't done Y"
  // 2. Noun + きり/っきり - "only X"
  // 3. 寝たきり - compound noun meaning "bedridden"

  r.either(
    // Pattern 1: Noun/Number + きり/っきり (particle case marker)
    // e.g., 一度きり, 一人きり, 二人っきり
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'NUM', 'PRON'] }, 'noun');
      const kiri = b.tok({
        textOneOf: ['きり', 'っきり'],
        dep: 'case'
      }, 'kiri');
      b.caseMarker(noun, kiri);
      b.captureSpan('きり', noun, kiri);
    },
    // Pattern 2: Verb + た/だ + きり/っきり (auxiliary particle)
    // e.g., 食べたきり, 行ったっきり, 喧嘩したっきり, 飲んだっきり
    // GiNZA parses this inconsistently:
    // - kiri pos: PART, ADP, SCONJ, AUX
    // - kiri dep: mark, aux, punct
    // - kiri head: points to the verb
    // - past aux: た or だ (for んだ contraction)
    (b) => {
      const verb = b.verb({}, 'verb');
      // Past tense auxiliary can be た or だ (for んだ constructions)
      const past = b.aux({ lemmaOneOf: ['た', 'だ'] }, 'past');
      b.auxOf(verb, past);
      const kiri = b.tok({
        textOneOf: ['きり', 'っきり'],
        depOneOf: ['aux', 'mark', 'punct']
      }, 'kiri');
      b.inOrder(past, kiri, 2);
      b.captureSpan('きり', verb, kiri);
    },
    // Pattern 3: 寝たきり (compound noun meaning "bedridden")
    // Tokenized as a single PROPN token
    (b) => {
      const netakiri = b.tok({
        textOneOf: ['寝たきり', '寝たっきり'],
        pos: 'PROPN'
      }, 'netakiri');
      b.capture(netakiri);
    }
  );
});
