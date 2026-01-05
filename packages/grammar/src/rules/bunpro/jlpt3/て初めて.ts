import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('て初めて', (r) => {
  // て初めて (te-form verb + 初めて): "only after doing X", "not until X"
  // Expresses that something happened only after a certain action or event for the first time.
  // Often conveys realization or discovery: "Only after (A) did I (B)"
  //
  // Structure: Verb[て-form] + 初めて
  //
  // GiNZA parsing:
  //   - Verb in て-form: VERB with various inflection forms + SCONJ (て/で) with dep=mark
  //   - For suru-verbs (勉強して): VERB(勉強) + AUX(し) + SCONJ(て), where te.head points to VERB
  //   - For regular verbs (失って): VERB(失う) + SCONJ(て), where te.head points to VERB
  //   - 初めて: ADV with lemma=初めて, dep=advmod
  //
  // Examples:
  //   日本語を勉強して初めて、他の国の言語で話す面白さを知った。
  //   失って初めて、その価値が分かりました。
  //   先生になって初めて、先生の大変さが分かった。
  //
  // Key insight: te.head points to the VERB (not the AUX), so we match VERB specifically.
  // We allow maxDistance=2 to handle suru-verbs: VERB + AUX(し) + SCONJ(て)

  const verbTe = r.verb({}, 'verbTe');
  const te = r.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
  const hajimete = r.tok({ lemmaOneOf: ['初めて', 'はじめて'], pos: 'ADV' }, 'hajimete');

  // te must point to verbTe
  r.headChild(verbTe, te, 'mark');

  // Order constraints
  // Allow maxDistance=4 to handle complex verb forms:
  // - Simple verb: VERB + SCONJ(て), distance=1
  // - Suru-verbs: VERB + AUX(し) + SCONJ(て), distance=2
  // - Verb-te-mite-te: VERB + SCONJ(て) + VERB(み) + SCONJ(て), distance=3
  // - Passive/causative: 指摘 + AUX(さ) + AUX(れ) + SCONJ(て), distance=3
  r.inOrder(verbTe, te, 4);
  r.inOrder(te, hajimete, 1);

  // Capture the full pattern
  r.captureSpan('て初めて', verbTe, hajimete);
});
