import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ようにいう - To tell, request, or order to do (in such a way)
 *
 * Matches verb + ように + 言う/頼む/命じる patterns for indirect speech,
 * requests, or orders.
 *
 * Structure:
 * - Verb (dictionary/negative/potential form) + ように + 言う (say/tell)
 * - Verb + ように + 言った (said/told - past)
 * - Verb + ように + 言われた (was told - passive)
 * - Verb + ように + 頼む (request)
 * - Verb + ように + 頼んだ (requested - past)
 * - Verb + ように + 頼まれた (was requested - passive)
 * - Verb + ように + 命じる (order)
 *
 * Meaning: "tell/ask/order someone to do something in a certain way"
 * This is a slightly formal way to state that something is said, requested,
 * or ordered in the manner of (A).
 *
 * Examples:
 * - 子供に水をこぼさないように言う (tell child not to spill water)
 * - 卵を買うように言った (told [someone] to buy eggs)
 * - トムに手伝いをするように頼んだ (requested Tom to help)
 * - 総理は国民に海外旅行に行かないように命じる (PM orders citizens not to travel abroad)
 * - お弁当を持って行くことを忘れないように言った (told not to forget to bring lunch)
 *
 * Key discriminators:
 * - The verb before ように is the content/aim of the speech act
 * - ように can be parsed as single token (SCONJ) or as よう (AUX) + に (AUX)
 * - Must be followed by a speech verb (言う, 頼む, 命じる, etc.)
 * - The content verb can be VERB (dictionary form) or ADJ (negative form like ない)
 *
 * Negative cases (should NOT match):
 * - Plain verb + 言う (without ように): 言うことは簡単だ (saying is easy)
 * - Noun + のように (similarity): 子供のように遊ぶ (play like a child)
 * - ように without speech verb: できるように勉強する (study to be able to)
 *
 * GiNZA parsing notes:
 * - Dictionary form verbs: VERB (e.g., 買う, する)
 * - Negative form verbs: ADJ (e.g., こぼさない has こぼさ as ADJ)
 * - よう is pos=AUX, lemma=よう
 * - に is pos=AUX, lemma=だ (copula auxiliary)
 * - The speech verbs conjugate normally (言う, 言った, 言われた, etc.)
 */
export default bunproLinguisticRule('ようにいう', (r) => {
  r.either(
    // Branch 1: ように as single token
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'verb');
      const yoni = b.tok({ text: 'ように' }, 'yoni');
      const speechVerb = b.verb({
        lemmaOneOf: ['いう', 'たのむ', 'めいじる']
      }, 'speechVerb');

      b.inOrder(verb, yoni, 5);
      b.inOrder(yoni, speechVerb, 10);
      b.captureSpan('ようにいう', verb, speechVerb);
    },
    // Branch 2: よう and に as separate tokens (most common in GiNZA)
    (b) => {
      const verb = b.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'verb');
      const you = b.aux({ lemma: 'よう' }, 'you');
      const ni = b.aux({ lemma: 'だ' }, 'ni');
      const speechVerb = b.verb({
        lemmaOneOf: ['いう', 'たのむ', 'めいじる']
      }, 'speechVerb');

      b.inOrder(verb, you, 5);
      b.inOrder(you, ni, 1);  // よう immediately followed by に
      b.inOrder(ni, speechVerb, 10);
      b.captureSpan('ようにいう', verb, speechVerb);
    }
  );
});
