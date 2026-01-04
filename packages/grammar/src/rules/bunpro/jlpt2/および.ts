import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: および (oyobi) - "and, as well as"
 *
 * A formal conjunction used to connect nouns, equivalent to "and" or "as well as".
 * Originally the conjunctive form (連用形) of the verb 及ぶ (oyobu - "to reach/extend to").
 * Used primarily in formal writing, official documents, business correspondence, and
 * notifications.
 *
 * Structure:
 * - Noun + および + Noun
 *
 * Examples:
 * - 免許証および印鑑を持ってきてください。
 *   (Please bring your driver's license and seal.)
 * - 夕食および朝食は別料金になります。
 *   (Dinner and breakfast are charged separately.)
 * - 当店の定休日は月曜日および水曜日となっています。
 *   (We are closed on Mondays as well as Wednesdays.)
 * - 顧客サポートおよび払戻に関する事項。
 *   (Matters related to refunds and customer support.)
 *
 * Key discriminators:
 * - Formal conjunction connecting two nouns (Noun + および + Noun)
 * - および is the conjunction itself (CCONJ in GiNZA)
 * - Similar to と (and) but much more formal
 * - Used in written documents, titles, and formal contexts
 * - Different from:
 *   - と (casual "and")
 *   - や (partial listing "and things like that")
 *   - そして (connects clauses/sentences)
 *   - 及ぶ (verb "to reach/extend")
 *
 * GiNZA parse structure:
 * - および is tagged as CCONJ (coordinating conjunction)
 * - Has dep=cc (conjunction) relation to connect nouns
 * - Typically appears between two nouns or noun phrases
 * - May also appear with dep=dep in some parses
 */
export default linguisticRule('および', (r) => {
  r.either(
    // Pattern 1: および as CCONJ with dep=cc (most common)
    (b) => {
      const oyobi = b.tok({
        textOneOf: ['および', '及び'],
        pos: 'CCONJ',
        dep: 'cc',
      }, 'oyobi');
      b.capture(oyobi);
    },

    // Pattern 2: および with dep=dep (alternative parse)
    (b) => {
      const oyobi = b.tok({
        textOneOf: ['および', '及び'],
        dep: 'dep',
      }, 'oyobi');
      b.capture(oyobi);
    },

    // Pattern 3: および as any CCONJ (covers variations)
    (b) => {
      const oyobi = b.tok({
        textOneOf: ['および', '及び'],
        pos: 'CCONJ',
      }, 'oyobi');
      b.capture(oyobi);
    }
  );
});
