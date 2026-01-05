import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: かたがた (katagata) - While doing, also doing; partly for the purpose of
 *
 * A formal conjunction used primarily in writing, especially for apologies,
 * thanks, and formal communications. Expresses that (A) is also partly for
 * the purpose of (B). Strong nuance of respect.
 *
 * Structures:
 * - Noun + かたがた (noun is often a verbal noun that would normally take する)
 * - Noun + する + かたがた (less common, する is usually omitted)
 *
 * Examples:
 * - お礼かたがた、そちらにお伺いをしたいと思っています。
 *   (I would like to visit you, partly for the purpose of thanking you.)
 * - 婚約の報告かたがた、お父様とお母様に会いに行きます。
 *   (I will go see my partner's parents, partly for the purpose of announcing our engagement.)
 * - 本棚の整理かたがた、春の大掃除をした。
 *   (I did spring cleaning, partly for the purpose of organizing my bookshelf.)
 * - お見舞いかたがた訪問しました。
 *   (I visited partly to express my sympathy.)
 *
 * Key discriminators:
 * - かたがた is a noun that acts as a conjunction
 * - Follows a noun (often a verbal noun like お礼, 報告, 挨拶, 訪問, お見舞い)
 * - Connects two actions where (A) is done partly for the purpose of (B)
 * - Formal register, used especially in writing
 * - Different from similar expressions like がてら (casual) or を兼ねて (less formal)
 *
 * GiNZA parse structure:
 * - Preceding noun (NOUN) + かたがた (NOUN or ADV)
 * - かたがた has various dependencies: compound, nmod, obl, advmod
 * - The key is the sequence: noun + かたがた
 */
export default linguisticRule('かたがた', (r) => {
  r.either(
    // Branch 1: Noun + かたがた (most common pattern)
    (b) => {
      const katagata = b.tok({
        text: 'かたがた',
        posOneOf: ['NOUN', 'ADV'],
      }, 'katagata');
      const noun = b.noun({}, 'noun');
      b.inOrder(noun, katagata, 1);
      b.captureSpan('かたがた', noun, katagata);
    },

    // Branch 2: Verb + かたがた (for cases like お見舞いかたがた)
    // Some verbal nouns are parsed as VERBS by GiNZA
    (b) => {
      const katagata = b.tok({
        text: 'かたがた',
        pos: 'ADV',
      }, 'katagata');
      const verb = b.verb({}, 'verb');
      b.inOrder(verb, katagata, 1);
      b.captureSpan('かたがた', verb, katagata);
    }
  );
});
