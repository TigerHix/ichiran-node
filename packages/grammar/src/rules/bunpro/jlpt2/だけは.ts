import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: だけは (dake wa) - "at least, if nothing else"
 *
 * Expresses doing something "as much as one can" or "at the very least".
 * The same verb is repeated before and after だけは, with the second verb
 * typically in past tense or ている form.
 *
 * Structure:
 * - Verb (dictionary/potential form) + だけは + Same verb (past/ている form)
 * - For する-verbs: Noun + だけは + する (verb repetition may be omitted)
 *
 * Examples:
 * - 食べるだけは食べた (I ate at least what I could)
 * - 飲めるだけは飲んだ (I drank as much as I could)
 * - 勉強だけはしたが、全然頭に情報が入ってこなかった (I did study, at least)
 * - 組合せを決めるだけは決めてみたが (I did decide on the combination, at least)
 *
 * Key discriminators:
 * - Same verb lemma appears before and after だけは
 * - Second verb typically in past form (た) or ている form
 * - First verb may be in potential form (れる/られる)
 * - For する-verbs, the verb may not be repeated (just noun + だけは + する)
 * - Often followed by が (but) indicating contrast with result
 *
 * Different from:
 * - Simple だけ (only/just) - no topic marker は
 * - だけで (only by/with) - different particle
 * - だけに (precisely because) - different nuance
 */
export default bunproLinguisticRule('だけは', (r) => {
  r.either(
    // Pattern 1: Verb (any form) + だけは + Verb (any form)
    // This covers most cases including potential forms
    // The structural pattern itself (same verb repeated around だけは) is the discriminator
    (b1) => {
      const verb1 = b1.verb({}, 'verb1');
      const dake = b1.tok({ lemma: 'だけ' }, 'dake');
      const wa = b1.particle('は', 'wa');
      const verb2 = b1.verb({}, 'verb2');

      b1.inOrder(verb1, dake, 5);
      b1.inOrder(dake, wa, 1);
      b1.inOrder(wa, verb2, 10);

      b1.captureSpan('だけは', verb1, verb2);
    },

    // Pattern 2: Noun + だけは + する (する-verb, verb not repeated)
    // For suru-verbs where only する appears after だけは
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const dake = b2.tok({ lemma: 'だけ' }, 'dake');
      const wa = b2.particle('は', 'wa');
      const suru = b2.verb({ lemma: 'する' }, 'suru');

      b2.inOrder(noun, dake, 3);
      b2.inOrder(dake, wa, 1);
      b2.inOrder(wa, suru, 5);

      b2.captureSpan('だけは', noun, suru);
    },

    // Pattern 3: Noun + する + だけは + する (full repetition for する-verbs)
    // Some sentences repeat the entire suru-verb
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const suru1 = b3.verb({ lemma: 'する' }, 'suru1');
      const dake = b3.tok({ lemma: 'だけ' }, 'dake');
      const wa = b3.particle('は', 'wa');
      const suru2 = b3.verb({ lemma: 'する' }, 'suru2');

      b3.inOrder(noun, suru1, 1);
      b3.inOrder(suru1, dake, 3);
      b3.inOrder(dake, wa, 1);
      b3.inOrder(wa, suru2, 10);

      b3.captureSpan('だけは', noun, suru2);
    },
  );
});
