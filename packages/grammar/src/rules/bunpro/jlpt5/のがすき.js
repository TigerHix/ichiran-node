import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('のがすき', (r) => {
  // Pattern: Verb (dictionary form) + の + が + 好き
  // Meaning: "Likes doing, enjoys doing" something
  // Examples:
  //   - 本を読むのが好き (likes reading books)
  //   - 映画を見るのが好き (likes watching movies)
  //   - サッカーをするのが好きだ (likes playing soccer)

  // Match verb in dictionary form (plain form)
  const verb = r.verb({ pos: 'VERB' }, 'verb');

  // の as nominalizer (turns verb phrase into noun)
  const no = r.particle('の', 'no');

  // が as subject marker
  const ga = r.particle('が', 'ga');

  // 好き (na-adjective meaning "to like")
  // GiNZA parses inconsistently, so match multiple variants
  const suki = r.tok({
    lemmaOneOf: ['すき', '好き'],
    posOneOf: ['NOUN', 'ADJ', 'VERB'],
  }, 'suki');

  // Structural constraints: verb + の + が + 好き
  r.inOrder(verb, no, 2);      // verb + の (nominalization)
  r.inOrder(no, ga, 1);        // の + が (subject marker)
  r.inOrder(ga, suki, 1);      // が + 好き (predicate)

  // Capture the full pattern
  r.captureSpan('のがすき', verb, suki);
});
