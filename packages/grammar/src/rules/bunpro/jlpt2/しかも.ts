import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: しかも (shikamo) - "moreover, furthermore, and on top of that"
 *
 * A conjunction that adds additional information to the previous statement,
 * often with an emphatic or surprising nuance. It indicates that both (A) and
 * (B) exist simultaneously or should be considered together.
 *
 * Structures:
 * - [Statement A]. しかも、[Statement B].
 * - [Statement A]、しかも [Statement B].
 * - しかも、[Statement B]. (at beginning of sentence)
 *
 * Examples:
 * - このテレビは画質がめちゃくちゃいい。しかも、受信機がついていないから受信料を払わなくてもいい。
 *   (This TV has very good picture quality. Moreover, it doesn't have a receiver so you don't have to pay the reception fee.)
 * - 彼女はとても頭がいいし性格もいい。しかも、美人だからもてないわけがない。
 *   (She is very smart and has a good personality. Moreover, she is beautiful so there's no way she's unpopular.)
 * - あの店は古いし汚い。しかも品揃えもよくないから、そのうち潰れるだろう。
 *   (That store is old and dirty. Moreover, they don't have good selection so they'll probably go out of business.)
 *
 * Key characteristics:
 * - Connects two clauses or sentences
 * - Adds information with emphatic nuance
 * - Often used for objective, observable facts
 * - Somewhat formal register
 * - Can be used at clause boundaries or sentence beginnings
 *
 * Kanji variants:
 * - しかも (hiragana - most common)
 * - 然も (kanji - rare)
 * - 而も (kanji - rare)
 * - 併も (kanji - rare)
 *
 * GiNZA parse structure:
 * - しかも (CCONJ) - conjunction, or ADV (adverb)
 * - dep=cc or dep=discourse (conjunction usage)
 *
 * Different from similar conjunctions:
 * - それに (soreni) - "and besides" (neutral addition, less emphatic)
 * - その上 (sono ue) - "furthermore" (more formal, no surprise implication)
 * - おまけに (omake ni) - "on top of that" (often negative, more colloquial)
 * - 更に (sara ni) - "further still" (written language, progression/escalation)
 * - また (mata) - "also, and again" (different position and particle requirements)
 * - なお (nao) - "furthermore" (neutral, formal, no emphasis)
 */
export default linguisticRule('しかも', (r) => {
  // しかも is a conjunction that can appear:
  // 1. Between clauses within a sentence
  // 2. At the beginning of a sentence
  // 3. Before or after punctuation (、)
  //
  // GiNZA typically parses しかも as:
  // - CCONJ (conjunction) or ADV (adverb)
  // - lemma = しかも (or kanji variants)
  // - dep = cc (coordination) or discourse
  //
  // The rule matches the word in all its written forms:
  // - しかも (standard hiragana)
  // - 然も, 而も, 併も (rare kanji variants)
  //
  // No additional structural constraints needed since しかも is a
  // standalone conjunction that doesn't depend on surrounding context
  // for grammatical validity.

  const shikamo = r.tok({
    textOneOf: ['しかも', '然も', '而も', '併も'],
    posOneOf: ['CCONJ', 'ADV', 'SCONJ'],
  }, 'shikamo');

  r.capture(shikamo);
});
