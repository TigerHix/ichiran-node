import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: しかも (shikamo) - "moreover, furthermore, and also"
 *
 * A somewhat formal conjunction used to add information with emphasis.
 * Indicates that (B) is also true in addition to the surprising or notable (A),
 * and both should be considered together. Similar to その上 but with more emphasis
 * and often implying the added information is unexpected or notable.
 *
 * Structures:
 * - [Statement A]. しかも、[Statement B].
 * - [Statement A]。しかも、[Statement B].
 * - [Statement A]、しかも[Statement B].
 *
 * Examples:
 * - このテレビは画質がめちゃくちゃいい。しかも、受信機がついていないから受信料を払わなくてもいい。
 *   (This TV has a very good resolution. And what's more, it doesn't have a receiver, so you don't have to pay a TV reception fee.)
 * - 彼女はとても頭がいいし性格もいい。しかも、美人だからもてないわけがない。
 *   (She is very smart and has a great personality. Moreover, she is beautiful, so there is no way that she is not popular.)
 * - あの店は古いし汚い。しかも品揃えもよくないから、そのうち潰れるだろう。
 *   (That store is old and dirty. Moreover, they don't have a good selection of goods, so they will probably go out of business soon.)
 * - 私の鼻はとても大きく、しかも曲がっている。
 *   (My nose is big and what's more, it is crooked.)
 *
 * Key characteristics:
 * - Somewhat formal register (but less formal than その上)
 * - Used at the beginning of sentences or between clauses
 * - Adds information with emphasis or surprise
 * - Focuses on objective, observable facts rather than subjective opinions
 * - Both (A) and (B) should be considered together
 *
 * Different from similar conjunctions:
 * - その上 (sono ue) - more formal, neutral addition
 * - それに (soreni) - "and besides" (neutral, casual)
 * - おまけに (omake ni) - "on top of that" (often negative, colloquial)
 * - 更に (sara ni) - "further still" (progression/escalation)
 * - なお (nao) - "furthermore" (neutral, formal, simple addition)
 *
 * Kanji variants:
 * - しかも (standard hiragana)
 * - 然も (rare kanji form)
 * - 而も (rare kanji form)
 * - 併も (rare kanji form)
 *
 * GiNZA parse structure:
 * - Usually parsed as a single ADV (adverb) or CCONJ (conjunction)
 * - dep=advmod or dep=cc
 * - Can appear at sentence start or between clauses
 */
export default bunproLinguisticRule('しかも', (r) => {
  // しかも is a conjunction meaning "moreover, furthermore, and also"
  // It adds information with emphasis, often implying the added information
  // is unexpected or notable.
  //
  // GiNZA typically parses しかも as:
  // - Single token: ADV (adverb) or CCONJ (conjunction)
  // - dep=advmod or dep=cc (conjunctive dependency)
  //
  // Key discriminators:
  // - POS: ADV or CCONJ (not particle, not noun)
  // - Must be しかも specifically (not し + か + も as separate particles)
  //
  // Common forms:
  // - しかも (standard hiragana form)
  // - Rare kanji forms: 然も, 而も, 併も
  //
  // Different from:
  // - しか〜ない ("only/not more than") - different grammar pattern
  // - Separate particles し + か + も in different contexts

  const shikamo = r.tok({
    text: 'しかも',
    posOneOf: ['ADV', 'CCONJ'],  // Adverb or conjunction
    depOneOf: ['advmod', 'cc', 'discourse'],  // Conjunctive dependencies
  }, 'shikamo');

  r.capture(shikamo);
});
