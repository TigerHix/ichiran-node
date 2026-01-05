import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: おそらく (osoraku) - "probably, perhaps, most likely, I fear that"
 *
 * A formal adverb indicating probability or presumption, often with a nuance of
 * concern or apprehension about the likelihood of something happening. It is
 * typically used with conjectural expressions like だろう, でしょう, かもしれない.
 *
 * Structures:
 * - おそらく + Phrase (modifies following predicate)
 * - 恐らく + Phrase (kanji form)
 *
 * Examples:
 * - 恐らく明日雨が降る。
 *   (Perhaps it will rain tomorrow. / I fear that it will rain tomorrow.)
 * - 恐らく彼はここにいないだろう。
 *   (He most likely isn't here.)
 * - 妻は今夜会社の人たちと飲み会に行くと言っていたので、おそらく１１時ごろぐらいまでは帰ってこないと思います。
 *   (My wife said she was going to a drinking party tonight with some people from work,
 *    so she probably won't be home until around 11:00 or so.)
 *
 * Key discriminators:
 * - Can be written as おそらく (hiragana) or 恐らく (kanji)
 * - Always functions as an adverb (ADV)
 * - Modifies the following predicate or entire clause
 * - More formal than たぶん (tabun)
 * - Higher certainty than もしかしたら (moshikashitara)
 * - Often used with conjectural endings (だろ, でしょう, etc.)
 *
 * GiNZA parse structure:
 * - おそらく/恐らく (ADV) - adverb
 * - Followed by predicate or modifies entire clause
 *
 * Different from:
 * - たぶん (tabun) - "probably" (less formal, everyday usage)
 * - もしかしたら (moshikashitara) - "perhaps, possibly" (lower certainty)
 * - まさか (masaka) - "surely not, no way" (expresses disbelief)
 * - ぜったい(に) (zettai ni) - "absolutely, definitely" (higher certainty)
 */
export default linguisticRule('おそらく', (r) => {
  // おそらく/恐らく (probably, perhaps, most likely, I fear that)
  // Formal adverb indicating probability, often with concern/apprehension
  // Can be written in hiragana (おそらく) or kanji (恐らく)
  // GiNZA tags this as ADV (adverb)

  const osoraku = r.tok({
    lemma: 'おそらく',
    pos: 'ADV',
  }, 'osoraku');

  // Capture just the adverb itself
  // It modifies whatever follows in the sentence
  r.capture(osoraku);
});
