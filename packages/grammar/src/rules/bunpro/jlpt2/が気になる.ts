import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: が気になる (ga ki ni naru) - "to be concerned about, to be interested in"
 *
 * An expression meaning "(something) is on my mind" or "(something) concerns me".
 * It indicates that something has naturally drawn the speaker's attention or interest.
 *
 * Structures:
 * - Noun + が + 気/き + に + なる (casual)
 * - Noun + が + 気/き + に + なります (polite)
 * - Noun + が + 気/き + に + なっている (progressive/state)
 *
 * Examples:
 * - この足跡が気になる。 (I am concerned about these tracks.)
 * - 値段が気になります。 (I am concerned about the price.)
 * - あの映画が気になっている。 (I am interested in that movie.)
 *
 * Note: Due to GiNZA tokenization variations, this rule may not match all instances.
 * Some sentences where が is absorbed into proper nouns (e.g., さんが)
 * or where なっている is tokenized differently may not match.
 */
export default linguisticRule('が気になる', (r) => {
  r.either(
    // Pattern 1: ...が + 気/き + に + なる (casual form)
    (b1) => {
      const ga = b1.particle('が', 'ga');
      const ki = b1.tok({ textOneOf: ['気', 'き'] }, 'ki');
      const ni = b1.particle('に', 'ni');
      const naru = b1.verb({ 
        textOneOf: ['なる', 'なります'],
      }, 'naru');

      b1.inOrder(ga, ki, 3);
      b1.inOrder(ki, ni, 1);
      b1.inOrder(ni, naru, 1);

      b1.captureSpan('が気になる', ga, naru);
    },

    // Pattern 2: ...が + 気/き + に + なっている (progressive)
    (b2) => {
      const ga = b2.particle('が', 'ga');
      const ki = b2.tok({ textOneOf: ['気', 'き'] }, 'ki');
      const ni = b2.particle('に', 'ni');
      const natte = b2.aux({ textOneOf: ['なって', 'になって'] }, 'natte');
      const iru = b2.aux({ textOneOf: ['いる', 'ってる'] }, 'iru');

      b2.inOrder(ga, ki, 3);
      b2.inOrder(ki, ni, 1);
      b2.inOrder(ni, natte, 1);
      b2.inOrder(natte, iru, 1);

      b2.captureSpan('が気になっている', ga, iru);
    }
  );
});
