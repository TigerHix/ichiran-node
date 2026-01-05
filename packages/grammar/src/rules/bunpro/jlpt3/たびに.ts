import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: たびに (every time/whenever)
 *
 * Pattern:
 * - Verb (dictionary form) + たびに = "every time I do X", "whenever I do X"
 * - Noun + の + たびに = "every time [noun], "whenever [noun]"
 *
 * Examples:
 * - 会うたびに (every time we meet)
 * - 見るたびに (every time I watch)
 * - 旅行のたびに (every time I travel)
 * - 休みのたびに (every holiday)
 *
 * Key patterns:
 * 1. Verb (dictionary form) + たびに
 * 2. Noun + の + たびに
 *
 * GiNZA parse structure:
 * - たび is a NOUN (meaning "time" or "occasion")
 * - に is a case particle (格助詞) marking the temporal point
 * - After verbs: verb modifies たび directly (dep=acl)
 * - After nouns: noun + の + たび (の has dep=case)
 *
 * Key discriminators vs ごとに/おきに:
 * - たびに: emphasizes each occurrence/event, "whenever X happens"
 * - ごとに: means "each/every" for regular intervals or items
 * - おきに: "at intervals of" for time/distance measurements
 */
export default bunproLinguisticRule('たびに', (r) => {
  // たび is a temporal noun (NOUN)
  // に is a case particle marking temporal reference
  const tabi = r.noun({ lemma: 'たび' }, 'tabi');
  const ni = r.particle('に', 'ni');
  r.caseMarker(tabi, ni);

  r.either(
    // Pattern 1: Verb (dictionary form) + たびに
    // The verb has dep=acl (adnominal clause) pointing to たび
    (b) => {
      const verb = b.tok({
        dep: 'acl',
      }, 'verb');
      b.inOrder(verb, tabi, 2);  // Allow auxiliaries between verb and たび
      b.captureSpan('たびに', verb, ni);
    },
    // Pattern 2: Noun + の + たびに
    // The noun has dep=nmod (nominal modifier) pointing to たび
    (b) => {
      const noun = b.noun({ dep: 'nmod' }, 'noun');
      const no = b.particle('の', 'no', { dep: 'case' });
      b.inOrder(noun, no, 1);
      b.inOrder(no, tabi, 1);
      b.captureSpan('たびに', noun, ni);
    }
  );
});
