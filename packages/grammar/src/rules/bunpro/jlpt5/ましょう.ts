import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ましょう', (r) => {
  // Polite volitional form: verb stem + ましょう (let's do)
  // Examples: 行きましょう (let's go), 食べましょう (let's eat), しましょう (let's do)
  //
  // Two patterns:
  //
  // Pattern 1: Regular verbs (VERB in 連用形-一般 + ましょう)
  //   - 行きましょう: 行き (VERB, 連用形-一般) + ましょう (AUX, 意志推量形)
  //   - 食べましょう: 食べ (VERB, 連用形-一般) + ましょう (AUX, 意志推量形)
  //
  // Pattern 2: Suru-verbs (VERB noun + し + ましょう)
  //   - 結婚しましょう: 結婚 (VERB) + し (AUX, する, 連用形-一般) + ましょう (AUX, 意志推量形)
  //   - 電話しましょう: 電話 (VERB) + し (AUX, する, 連用形-一般) + ましょう (AUX, 意志推量形)

  r.either(
    // Pattern 1: Regular verbs with stem form
    (b1) => {
      const verb = b1.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mashou = b1.aux({
        lemma: 'ます',
        inflectionForm: '意志推量形',
      }, 'mashou');
      b1.auxOf(verb, mashou);
      b1.captureSpan('ましょう', verb, mashou);
    },

    // Pattern 2: Suru-verbs (noun verb + し + ましょう)
    (b2) => {
      const nounVerb = b2.verb({
        // No inflectionForm for suru-verb nouns
      }, 'verb');
      const shi = b2.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般',
      }, 'shi');
      const mashou = b2.aux({
        lemma: 'ます',
        inflectionForm: '意志推量形',
      }, 'mashou');
      b2.auxOf(nounVerb, shi);
      b2.auxOf(nounVerb, mashou);
      b2.captureSpan('ましょう', nounVerb, mashou);
    }
  );
});
