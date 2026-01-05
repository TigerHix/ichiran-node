import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: きらいがある (kirai ga aru) - Have a tendency to, Tend to, Be slightly~
 *
 * Expresses a negative tendency or fault. Indicates that something has an
 * undesirable inclination or is prone to (A). More formal and critical than
 * similar expressions like がち or 傾向がある.
 *
 * Structures:
 * - Verb［る］+ きらいがある (dictionary form)
 * - Verb［ない］+ きらいがある (negative form)
 * - Noun + の + きらいがある
 *
 * Examples:
 * - 言い過ぎるきらいがある
 *   (Has a tendency to say too much)
 * - 独断するきらいがある
 *   (Has a tendency to act arbitrarily)
 * - 過保護のきらいがある
 *   (Has a tendency to be overprotective)
 * - 意見を言わないきらいがある
 *   (Has a tendency to not state opinions)
 *
 * Key discriminators:
 * - きらい is a NOUN (meaning "dislike" or "fault")
 * - が is the subject particle (ADP with dep: case)
 * - ある is the verb "to exist" (VERB)
 * - Captures from the preceding verb/noun to ある
 * - Used only for negative tendencies
 *
 * GiNZA parse structure:
 * - Verb/Noun + きらい(NOUN) + が(ADP) + ある(VERB)
 * - The particle が typically has dep: case marking きらい
 * - The verb ある is the main predicate
 */
export default linguisticRule('きらいがある', (r) => {
  r.either(
    // Branch 1: Verb (dictionary form or negative) + きらいがある
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'verb');
      const kirai = b.tok({
        lemma: 'きらい',
        pos: 'NOUN',
      }, 'kirai');
      b.inOrder(verb, kirai, 3);

      const ga = b.particle('が', 'ga');
      b.inOrder(kirai, ga, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(ga, aru, 1);

      b.captureSpan('きらいがある', verb, aru);
    },

    // Branch 2: Noun + の + きらいがある
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      const kirai = b.tok({
        lemma: 'きらい',
        pos: 'NOUN',
      }, 'kirai');
      b.inOrder(no, kirai, 1);

      const ga = b.particle('が', 'ga');
      b.inOrder(kirai, ga, 1);

      const aru = b.verb({ lemma: 'ある' }, 'aru');
      b.inOrder(ga, aru, 1);

      b.captureSpan('きらいがある', noun, aru);
    }
  );
});
