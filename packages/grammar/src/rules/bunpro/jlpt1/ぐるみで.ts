import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: ぐるみで (suffix meaning "the whole X", "including all of X")
 *
 * Noun + ぐるみ + で/の = the whole group/institution, -wide, all of
 * Examples:
 * - 家族ぐるみで初詣に行きます (go to Hatsumode with the entire family)
 * - 会社ぐるみで違法な取引をしていた (doing illegal business as a whole company)
 * - 家族ぐるみの付き合い (relationship with the whole family)
 * - 街ぐるみの年中行事 (town-wide annual event)
 *
 * Pattern variations:
 * 1. Noun + ぐるみで (instrumental/agentive: with the whole X)
 * 2. Noun + ぐるみの (modifying: X-wide/entire X's [noun])
 *
 * ぐるみ is a suffix from the verb 包む (to wrap/envelop).
 * It indicates that something encompasses ALL of the preceding noun/group.
 */
export default bunproLinguisticRule('ぐるみで', (r) => {
  r.either(
    // Pattern 1: Noun + ぐるみで (instrumental/agentive usage)
    (b) => {
      const noun = b.noun({}, 'noun');

      // ぐるみ can be parsed as:
      // - Single NOUN token (lemma=ぐるみ)
      // - May appear as part of compound noun in GiNZA
      const gurumi = b.tok({
        textOneOf: ['ぐるみ', '包み'],
        lemmaOneOf: ['ぐるみ', '包み'],
        pos: 'NOUN',
      }, 'gurumi');

      b.inOrder(noun, gurumi, 1);

      // で particle (instrumental/agentive marker)
      const de = b.particle('で', 'de');
      b.inOrder(gurumi, de, 1);

      b.captureSpan('ぐるみで', noun, de);
    },

    // Pattern 2: Noun + ぐるみの (modifying usage)
    (b) => {
      const noun = b.noun({}, 'noun');

      const gurumi = b.tok({
        textOneOf: ['ぐるみ', '包み'],
        lemmaOneOf: ['ぐるみ', '包み'],
        pos: 'NOUN',
      }, 'gurumi');

      b.inOrder(noun, gurumi, 1);

      // の particle (modifying marker)
      const no = b.particle('の', 'no');
      b.inOrder(gurumi, no, 1);

      b.captureSpan('ぐるみで', noun, no);
    }
  );
});
