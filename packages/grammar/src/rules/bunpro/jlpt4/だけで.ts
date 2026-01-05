import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: だけで - Just by / Just with / Only with
 *
 * Matches noun or verb + だけで to express that something happens "only with" or "just by" doing something.
 *
 * Structures:
 * - Noun + だけで (just with/using only noun)
 * - Verb + だけで (just by doing verb)
 *
 * Examples:
 * - これだけで (just with this)
 * - 電子レンジだけで (just with a microwave)
 * - 家族と一緒にいるだけで (just by being with family)
 * - 見るだけで (just by watching)
 * - 言ってくれるだけで (just by saying)
 *
 * Key discriminators:
 * - だけ is the adverbial particle (lemma=だけ)
 * - で must have dep=case OR dep=aux (instrumental means or copula te-form)
 * - This excludes だけでなく where で has dep=fixed
 *
 * GiNZA parse structure:
 * - POSITIVE: パーティーに行くだけで楽しい
 *   - 行く(VERB) + だけ(PART) + で(ADP, lemma=で, dep=case, head=2→VERB)
 * - POSITIVE: 電子レンジだけでオーブンの機能はついていない
 *   - 電子レンジ(NOUN) + だけ(ADP) + で(ADP, lemma=で, dep=case, head=0→NOUN)
 * - POSITIVE: 顔を見ただけで、彼はいい人だと分かった
 *   - 見る(VERB) + た(AUX) + だけ(PART) + で(AUX, lemma=だ, dep=aux, head=2→VERB)
 * - NEGATIVE (だけでなく): 日本語だけでなく英語も話せる
 *   - 日本語(NOUN) + だけ(ADP) + で(AUX, lemma=だ, dep=fixed)
 *
 * Note: The pattern matches both instrumental "means" (行くだけで = just by going) and
 * locative "location" (電子レンジだけで = just with a microwave), which are both valid
 * usages of this grammar point.
 */
export default bunproLinguisticRule('だけで', (r) => {
  const dake = r.tok({ lemma: 'だけ' }, 'dake');

  // で must have dep=case OR dep=aux to exclude だけでなく (dep=fixed)
  const de = r.tok({ text: 'で', depOneOf: ['case', 'aux'] }, 'de');

  // だけ and で must be adjacent
  r.inOrder(dake, de, 1);

  // Capture from だけ through で
  r.captureSpan('だけで', dake, de);
});
