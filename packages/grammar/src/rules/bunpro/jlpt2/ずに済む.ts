import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ずに済む (zuni sumu) - Can resolve without, can do without
 *
 * A somewhat old-fashioned expression indicating that someone got away with
 * not doing (A) or that (A) came to an end without happening. Literally means
 * "it ends without doing (A)".
 *
 * The pattern combines:
 * - ずに (zu ni): Classical/literary form of "without doing" (negative auxiliary)
 * - 済む (sumu): To finish, to end, to be resolved
 *
 * Structures:
 * - Verb［negative stem］+ ずに + 済む (casual)
 * - Verb［negative stem］+ ずに + 渡みます (polite)
 * - Also seen with: ないで済む, なくて済む, なしで済む
 *
 * Formation:
 * - Remove ない from the negative form and attach ずに + 済む
 * - Five-grade verbs: やらない → やらずに済む
 * - Ichidan verbs: 食べない → 食べずに済む
 * - Irregular verbs:
 *   - する → せずに済む (NOT しずに済む)
 *   - 来る → 来ずに済む
 *
 * Examples:
 * - 今宿題をやっておけば後でやらずに済むから、今のうちにやっておこう。
 *   (If I do my homework now, I can get by without doing it later, so I'll do it now.)
 * - このVIPパスを使えば待たずに済みます。
 *   (If you have this VIP pass, you can get by without waiting.)
 * - 犯人は警察から、捕まらずに済んだ。
 *   (The criminal got away without being arrested by the police.)
 * - 運転士の巧みな技術のおかげで脱線せずに済んだ。
 *   (Thanks to the clever skill of the conductor, they got by without derailing.)
 * - 知人の紹介で入社できたので、テストは受けずに済んだ。
 *   (Since I was able to join the company through the introduction of a friend,
 *    I got by without taking a test.)
 * - 芸能人がツイートしてくれたから、宣伝費を使わずに済んだ。
 *   (Because an entertainer tweeted for me, I got by without having to use
 *    advertising expenses.)
 *
 * Key discriminators:
 * - ず (zu) is the classical negative auxiliary (equivalent to ない/ぬ)
 * - Follows verb stem in negative form (連用形 after removing ない)
 * - に (ni) is an adverbial particle
 * - 済む (sumu) is a godan verb meaning "to finish, to end, to be resolved"
 * - Expresses relief that something unpleasant was avoided
 * - Different from:
 *   - ずに alone (just "without doing", no resolution aspect)
 *   - ないで済む (modern equivalent, less formal)
 *   - なくて済む (te-form variant)
 *   - なしで済む (can be used with nouns)
 *
 * GiNZA parse structure:
 * - Verb stem + ずに(AUX/PART/SCONJ) + に(ADP) + 済む(VERB/AUX)
 * - ず may have dep=aux, dep=mark, dep=case, or dep=fixed
 * - に is typically ADP with dep=case
 * - 済む may be VERB or AUX depending on conjugation
 * - Various dependency relations between components
 */
export default linguisticRule('ずに済む', (r) => {
  r.either(
    // Pattern 1: ずに as single token (AUX) + 済む/すむ
    // Most common pattern where GiNZA parses "ずに" as one auxiliary token
    (b1) => {
      const zuni = b1.tok({
        text: 'ずに',
        posOneOf: ['AUX', 'PART', 'SCONJ'],
      }, 'zuni');
      const sumu = b1.tok({
        textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'],
        posOneOf: ['VERB', 'AUX'],
      }, 'sumu');

      b1.inOrder(zuni, sumu, 3);
      b1.captureSpan('ずに済む', zuni, sumu);
    },

    // Pattern 2: ず (AUX) + に (ADP) + 済む/すむ
    // GiNZA parses ず and に as separate tokens
    (b2) => {
      const zu = b2.tok({
        text: 'ず',
        posOneOf: ['AUX', 'PART', 'SCONJ'],
      }, 'zu');
      const ni = b2.particle('に', 'ni');
      const sumu = b2.tok({
        textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'],
        posOneOf: ['VERB', 'AUX'],
      }, 'sumu');

      b2.inOrder(zu, ni, 1);
      b2.inOrder(ni, sumu, 2);
      b2.captureSpan('ずに済む', zu, sumu);
    },

    // Pattern 3: ずに + 済む/すむ with aux dependency
    // ずに is auxiliary attached to 済む
    (b3) => {
      const zuni = b3.tok({
        text: 'ずに',
      }, 'zuni');
      const sumu = b3.tok({
        textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'],
      }, 'sumu');

      b3.inOrder(zuni, sumu, 3);
      b3.headChild(sumu, zuni, 'aux');
      b3.captureSpan('ずに済む', zuni, sumu);
    },

    // Pattern 4: ず + に + 済む/すむ with fixed/compound dependencies
    // All three tokens form a fixed expression
    (b4) => {
      const zu = b4.tok({
        text: 'ず',
      }, 'zu');
      const ni = b4.particle('に', 'ni');
      const sumu = b4.tok({
        textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'],
      }, 'sumu');

      b4.inOrder(zu, ni, 1);
      b4.inOrder(ni, sumu, 2);
      b4.headChild(sumu, zu, 'fixed');
      b4.headChild(sumu, ni, 'fixed');
      b4.captureSpan('ずに済む', zu, sumu);
    },

    // Pattern 5: Catch-all with loose constraints
    // For unexpected GiNZA parsings
    (b5) => {
      const zu = b5.tok({
        textOneOf: ['ず', 'ずに'],
      }, 'zu');
      const sumu = b5.tok({
        textOneOf: ['済む', 'すむ', '済み', 'すみ', '済んだ', 'すんだ', '済みます', 'すみます', '済みました', 'すみました'],
      }, 'sumu');

      b5.inOrder(zu, sumu, 5);
      b5.captureSpan('ずに済む', zu, sumu);
    }
  );
});
