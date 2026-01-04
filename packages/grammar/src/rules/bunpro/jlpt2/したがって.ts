import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: したがって (shitagatte) - Therefore, Thus, As a result
 *
 * A formal conjunction used to indicate that (B) follows logically from (A).
 * "In compliance with (A), (B)."
 *
 * Structure:
 * - (Cause) + したがって + (Result)
 *
 * Can be written in hiragana (したがって) or kanji (従って)
 *
 * Examples:
 * - 彼は長男です。したがって、次期社長はおそらく彼でしょう。
 *   (He is the oldest son. Therefore, he will soon be the CEO, right?)
 * - 雨が降っている。したがって、試合は中止だ。
 *   (It is raining. Therefore, the game is cancelled.)
 * - 本番直前です。したがって、彼は極度に緊張しています。
 *   (It is right before the big game. Therefore, he is at maximum nervousness.)
 *
 * Key discriminators:
 * - したがって is a conjunction (接続詞)
 * - Appears at the beginning of a sentence
 * - More formal than だから, それで, etc.
 * - Can be written as 従って (kanji) or したがって (hiragana)
 * - GiNZA parses したがって as CONJ or ADV depending on context
 *
 * GiNZA parse structure:
 * - したがって(CONJ) or したがって(ADV)
 * - 従って(CONJ) or 従って(ADV)
 */
export default linguisticRule('したがって', (r) => {
  // Match したがって or 従って as a conjunction
  // Both hiragana and kanji forms are used
  const shitagatte = r.tok({
    textOneOf: ['したがって', '従って', '従（したが）って'],
    posOneOf: ['CONJ', 'ADV', 'SCONJ'],
  }, 'shitagatte');

  r.captureSpan('したがって', shitagatte, shitagatte);
});
