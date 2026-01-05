import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: といい - it would be good if / I hope / I wish
 *
 * Expresses hope for oneself or others. A casual phrase meaning "it would be good if".
 *
 * Structure:
 * - Verb/Adj (dictionary form) + と + いい
 * - Na-adj/Noun + だ + と + いい
 *
 * Often followed by:
 * - な / なぁ (hoping for oneself)
 * - ね (hoping for others)
 * - んだけど / んですが (wishing with reservation)
 * - です (polite)
 *
 * Examples:
 * - 明日は晴れるといいな。
 *   (I hope it clears up tomorrow.)
 * - 今夜のパーティーは楽しいといいね！
 *   (I hope your party tonight is fun!)
 * - 最近は忙しいから、今週末は休みだといいのだが。
 *   (I've been busy lately, so I hope I have this weekend off.)
 *
 * This is different from:
 * - Quotation と (followed by 言う/思う, etc. - not いい)
 * - Conditional と where いい modifies a noun (e.g., いい人, いい天気)
 *
 * GiNZA parse structure:
 * - After verbs: と is SCONJ with dep=mark
 * - After adjectives/nouns+だ: と may be ADP with dep=case or mark
 * - Followed by いい (ADJ with lemma=いい)
 *
 * Key discriminators:
 * - In hope pattern: いい is the root or has aux/copula children ( sentence-final)
 * - NOT hope pattern: いい has amod dependency to a following noun
 *
 * The pattern appears:
 * - After verbs in dictionary form (e.g., 晴れると)
 * - After i-adjectives (e.g., 楽しいと)
 * - After na-adjectives/nouns + だ (e.g., 休みだと, 綺麗だと)
 */
export default bunproLinguisticRule('といい', (r) => {
  // いい (good/would be good) - the key distinguishing word
  // This MUST be lemma=いい to avoid matching other adjectives
  const ii = r.tok({
    lemma: 'いい'
  }, 'ii');

  // Handle different patterns for と
  r.either(
    // Pattern 1: と after verbs (SCONJ with dep=mark)
    (branch1) => {
      const to = branch1.tok({
        text: 'と',
        pos: 'SCONJ',
        dep: 'mark'
      }, 'to');

      branch1.inOrder(to, ii, 1);
      branch1.captureSpan('といい', to, ii);
    },
    // Pattern 2: と after adjectives/nouns (ADP with dep=case or mark)
    (branch2) => {
      const to = branch2.tok({
        text: 'と',
        depOneOf: ['case', 'mark']
      }, 'to');

      branch2.inOrder(to, ii, 1);
      branch2.captureSpan('といい', to, ii);
    }
  );
});
