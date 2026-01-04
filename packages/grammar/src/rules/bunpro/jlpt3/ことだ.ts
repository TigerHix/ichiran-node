import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ことだ - should / ought to (advice)
 *
 * Matches verb (dictionary or negative form) + こと + だ/です
 *
 * This is used to give advice or make suggestions with an authoritative tone,
 * as if stating a rule or general expectation.
 *
 * Structure:
 * - Verb［る］+ こと + だ/です (casual/polite)
 * - Verb［ない］+ こと + だ/です (casual/polite)
 *
 * Examples:
 * - 時間通りに来ることだ (You should come on time)
 * - 朝ご飯を食べることだ (You should eat breakfast)
 * - 諦めないことだ (You should not give up)
 * - 練習することです (You should practice)
 *
 * GiNZA parse structure:
 * - 来ることだ: 来る(verb) + こと(noun) + だ(aux/copula)
 * - 食べることだ: 食べる(verb) + こと(noun) + だ(aux/copula)
 * - 諦めないことだ: 諦め(verb) + ない(aux) + こと(noun) + だ(aux/copula)
 * - 練習することです: 練習(verb/aux) + する(verb) + こと(noun) + です(aux/copula)
 *
 * This is different from:
 * - たことがある (JLPT5) - past experience "have done before"
 * - ことがある (JLPT3) - "sometimes do" or "there are times when"
 * - ことはない (JLPT3) - "there is no need to" or "never happens"
 * - ことになる (JLPT3) - "it is decided that"
 * - ことにする (JLPT3) - "decide to"
 * - ものだ (JLPT3) - "supposed to" or "that's how it is"
 */
export default linguisticRule('ことだ', (r) => {
  r.either(
    // Branch 1: Verb dictionary form + ことだ (casual)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const da = b.aux({ lemma: 'だ' }, 'copula');
      b.copulaOf(koto, da);
      b.inOrder(koto, da, 1);

      b.captureSpan('ことだ', verb, da);
    },
    // Branch 2: Verb dictionary form + ことです (polite)
    (b) => {
      const verb = b.verb({}, 'pred');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(verb, koto, 1);

      const desu = b.aux({ lemma: 'です' }, 'copula');
      b.copulaOf(koto, desu);
      b.inOrder(koto, desu, 1);

      b.captureSpan('ことだ', verb, desu);
    },
    // Branch 3: Verb negative form + ないことだ (casual)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(verb, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto, 1);

      const da = b.aux({ lemma: 'だ' }, 'copula');
      b.copulaOf(koto, da);
      b.inOrder(koto, da, 1);

      b.captureSpan('ことだ', verb, da);
    },
    // Branch 4: Verb negative form + ないことです (polite)
    (b) => {
      const verb = b.verb({}, 'verb');
      const nai = b.aux({ lemma: 'ない' }, 'nai');
      b.auxOf(verb, nai);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(nai, koto, 1);

      const desu = b.aux({ lemma: 'です' }, 'copula');
      b.copulaOf(koto, desu);
      b.inOrder(koto, desu, 1);

      b.captureSpan('ことだ', verb, desu);
    }
  );
});
