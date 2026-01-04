import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ようにする (you-ni-suru) - To try to, To make sure to, To endeavor to
 *
 * Expresses volition or effort to change behavior. Literally means "to do in such a way that X happens."
 * Indicates making an effort to ensure something does/doesn't happen.
 *
 * Structure:
 * - Verb[dictionary form/negative form] + ように + する (make an effort to do/not do something)
 *
 * Examples:
 * - 毎日走るようにしている (try to run every day / make a habit of running)
 * - 遅れないようにしてください (please try not to be late)
 * - 成績が悪いから学校を休まないようにする (will try not to miss school)
 * - 間に合うようにしましょう (let's try to be on time)
 * - そのコップを落とさないようにしてください (please try not to drop that cup)
 *
 * Conjugations of する:
 * - Present: ようにする / ようにします
 * - Past: ようにした / ようにしました
 * - Te-form: ようにして / ようして (request)
 * - Progressive: ようにしている / ようしてる (habitual effort)
 * - Volitional: ようにしよう / ようにしましょう
 *
 * Key discriminators:
 * - Follows verb (dictionary form or negative form)
 * - ように can be single token or よう + に
 * - Followed by する (or its conjugations)
 * - Expresses effort/intention (not just purpose like ように)
 *
 * Negative examples to avoid:
 * - Plain する without ように (just "to do")
 * - ようになる (change of state: "came to be that...")
 * - ようだ (seems like)
 *
 * GiNZA parsing notes:
 * - ように often parsed as single token (text=ように, pos=SCONJ)
 * - Sometimes parsed as よう (pos=NOUN) + に (pos=ADP/particle)
 * - する appears as verb with various auxiliaries for conjugations
 */
export default linguisticRule('ようにする', (r) => {
  r.either(
    // Branch 1: ように as single token + various する conjugations
    (b1) => {
      const verb = b1.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const yoni = b1.tok({ text: 'ように' }, 'yoni');
      const suru = b1.verb({ lemma: 'する' }, 'suru');

      b1.inOrder(verb, yoni, 5);
      b1.inOrder(yoni, suru, 3);
      b1.captureSpan('ようにする', verb, suru);
    },

    // Branch 2: よう + に as separate tokens + various する conjugations
    (b2) => {
      const verb = b2.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const you = b2.tok({ text: 'よう' }, 'you');
      const ni = b2.particle('に', 'ni');
      const suru = b2.verb({ lemma: 'する' }, 'suru');

      b2.inOrder(verb, you, 5);
      b2.inOrder(you, ni, 1);
      b2.inOrder(ni, suru, 3);
      b2.captureSpan('ようにする', verb, suru);
    }
  );
});
