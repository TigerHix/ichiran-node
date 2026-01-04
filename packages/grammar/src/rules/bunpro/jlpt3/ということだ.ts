import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: ということだ - it means that / I hear that / it is said that
 *
 * Matches phrase + という + こと + だ/です
 *
 * This pattern nominalizes an entire phrase and asserts it as a fact or hearsay.
 * Used to report information from others, summarize situations, or draw conclusions.
 *
 * Structure:
 * - Phrase/Sentence + と + いう + こと + だ (casual)
 * - Phrase/Sentence + と + いう + こと + です (polite)
 * - Phrase/Sentence + と + の + こと + だ (shortened hearsay form)
 *
 * Examples:
 * - 先生によると、この病気は薬では治せないということだ。
 *   (According to the doctor, it is said that this sickness cannot be cured with medicine.)
 * - 人によって考え方が違うということだ。
 *   (It is said that different people have different ways of thinking.)
 * - 地域によってルールが違うとのことだ。
 *   (It is said that the rules differ depending on the area.)
 *
 * This is different from:
 * - ということ (JLPT4) - just nominalization "the fact that"
 * - ことだ (JLPT3) - giving advice "you should"
 * - そうだ (JLPT3) - hearsay but less formal
 * - だって (JLPT3) - casual hearsay
 *
 * GiNZA parse structure:
 * - ということだ: と(particle) + いう(verb) + こと(noun) + だ(aux/copula)
 * - ということです: と(particle) + いう(verb) + こと(noun) + です(aux/copula)
 * - とのことだ: と(particle) + の(particle/noun) + こと(noun) + だ(aux/copula)
 */
export default linguisticRule('ということだ', (r) => {
  // Quote particle と (marks the quoted phrase)
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: ということだ (casual - "it means that / I hear that")
    (b) => {
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const da = b.aux({ lemma: 'だ' }, 'copula');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, koto, 1);
      b.copulaOf(koto, da);
      b.inOrder(koto, da, 1);

      b.captureSpan('ということだ', to, da);
    },
    // Pattern 2: ということです (polite)
    (b) => {
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const desu = b.aux({ lemma: 'です' }, 'copula');

      b.inOrder(to, iu, 1);
      b.inOrder(iu, koto, 1);
      b.copulaOf(koto, desu);
      b.inOrder(koto, desu, 1);

      b.captureSpan('ということだ', to, desu);
    },
    // Pattern 3: とのことだ (shortened hearsay form - "they say that")
    (b) => {
      // GiNZA might tokenize の as noun or particle, so we use generic tok
      const no = b.tok({ text: 'の' }, 'no');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const da = b.aux({ lemma: 'だ' }, 'copula');

      b.inOrder(to, no, 1);
      b.inOrder(no, koto, 1);
      b.copulaOf(koto, da);
      b.inOrder(koto, da, 1);

      b.captureSpan('ということだ', to, da);
    },
    // Pattern 4: とのことです (shortened hearsay form, polite)
    (b) => {
      const no = b.tok({ text: 'の' }, 'no');
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      const desu = b.aux({ lemma: 'です' }, 'copula');

      b.inOrder(to, no, 1);
      b.inOrder(no, koto, 1);
      b.copulaOf(koto, desu);
      b.inOrder(koto, desu, 1);

      b.captureSpan('ということだ', to, desu);
    }
  );
});
