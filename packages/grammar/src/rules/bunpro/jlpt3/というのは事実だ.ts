import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT3: というのは事実だ - the fact is that / it is a fact that
 *
 * Matches phrase + という + の + は + 事実/じじつ + だ/です
 *
 * This pattern strongly emphasizes that something is a fact or truth.
 * It's used to make emphatic statements about objective facts.
 *
 * Structure:
 * - Phrase/Sentence + と + いう + の + は + 事実/じじつ + だ (casual)
 * - Phrase/Sentence + と + いう + の + は + 事実/じじつ + です (polite)
 * - Phrase/Sentence + の + は + 事実 + だ (shortened form)
 *
 * Note: Test data uses hiragana "じじつ" when preceded by という,
 * and kanji "事実" in the shortened form (without という).
 *
 * Examples:
 * - 彼が俳優だというのはじじつだ。
 *   (It is a fact that he is an actor.)
 * - 地球が丸いというのはじじつだ。
 *   (It is a fact that the earth is round.)
 * - 彼女と仲直りしたのは事実だ。
 *   (It is a fact that I made up with her.)
 *
 * This is different from:
 * - ということだ (JLPT3) - "it means that / I hear that" (hearsay)
 * - というのは (JLPT3) - nominalization "what is called ~"
 * - に違いない (JLPT3) - strong conviction but not absolute certainty
 *
 * GiNZA parse structure:
 * - というのはじじつだ: と(particle) + いう(verb) + の(noun/particle) + は(particle) + じじつ(noun) + だ(aux/copula)
 * - というのは事実だ: と(particle) + いう(verb) + の(noun/particle) + は(particle) + 事実(noun) + だ(aux/copula)
 * - のは事実だ: の(noun/particle) + は(particle) + 事実(noun) + だ(aux/copula)
 */
export default bunproLinguisticRule('というのは事実だ', (r) => {
  r.either(
    // Pattern 1: というのはじじつだ / というのは事実だ (casual - "the fact is that")
    // Note: Try matching by text only first, to handle hiragana vs kanji
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const jijitsu = b.tok({ textOneOf: ['事実', 'じじつ'] }, 'jijitsu');
      const da = b.aux({ lemma: 'だ' }, 'copula');

      b.inOrder(to, iu, 5); // Allow up to 5 tokens between と and 事実
      b.inOrder(iu, jijitsu, 4); // Allow up to 4 tokens between いう and 事実
      b.inOrder(jijitsu, da, 1);

      b.captureSpan('というのは事実だ', to, da);
    },
    // Pattern 2: というのはじじつです / というのは事実です (polite)
    // Note: Try matching by text only first, to handle hiragana vs kanji
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      const jijitsu = b.tok({ textOneOf: ['事実', 'じじつ'] }, 'jijitsu');
      const desu = b.aux({ lemma: 'です' }, 'copula');

      b.inOrder(to, iu, 5); // Allow up to 5 tokens between と and 事実
      b.inOrder(iu, jijitsu, 4); // Allow up to 4 tokens between いう and 事実
      b.inOrder(jijitsu, desu, 1);

      b.captureSpan('というのは事実だ', to, desu);
    },
    // Pattern 3: のは事実だ (shortened form - weaker emphasis, uses kanji)
    (b) => {
      const no = b.tok({ textOneOf: ['の'], posOneOf: ['NOUN', 'PART', 'SCONJ'] }, 'no');
      const wa = b.particle('は', 'wa');
      const jijitsu = b.tok({ textOneOf: ['事実', 'じじつ'] }, 'jijitsu');
      const da = b.aux({ lemma: 'だ' }, 'copula');

      b.inOrder(no, wa, 1);
      b.inOrder(wa, jijitsu, 1);
      b.inOrder(jijitsu, da, 1);

      b.captureSpan('というのは事実だ', no, da);
    },
    // Pattern 4: のは事実です (shortened form, polite, uses kanji)
    (b) => {
      const no = b.tok({ textOneOf: ['の'], posOneOf: ['NOUN', 'PART', 'SCONJ'] }, 'no');
      const wa = b.particle('は', 'wa');
      const jijitsu = b.tok({ textOneOf: ['事実', 'じじつ'] }, 'jijitsu');
      const desu = b.aux({ lemma: 'です' }, 'copula');

      b.inOrder(no, wa, 1);
      b.inOrder(wa, jijitsu, 1);
      b.inOrder(jijitsu, desu, 1);

      b.captureSpan('というのは事実だ', no, desu);
    }
  );
});
