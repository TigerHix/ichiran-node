import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: どころではない (dokoro dewa nai) - "far from, let alone, out of the question"
 *
 * An expression emphasizing that something is hardly the situation or place for (A),
 * or that (A) is far from the actual reality. It expresses that something is completely
 * out of the question due to circumstances.
 *
 * Structure:
 * - Verb (dictionary form) + どころではない/どころじゃない
 * - I-adjective + どころではない/どころじゃない
 * - Noun + どころではない/どころじゃない
 *
 * Polite variants:
 * - + どころではありません/どころじゃありません
 * - + どころではないです/どころじゃないです
 *
 * Past tense variants:
 * - + どころではなくなった/どころじゃなくなった
 *
 * Examples:
 * - 仕事が忙しすぎて、年休を取るどころではない。
 *   (I'm so busy at work, taking time off is out of the question.)
 * - 出産は痛いどころではないらしい。
 *   (They say childbirth is far from painful.)
 * - 今は仕事が忙しいし、お金も全然ないから、結婚どころじゃない。
 *   (Since I'm busy with work and have no money, it's not the time to get married.)
 *
 * Key discriminators:
 * - Expresses strong negative evaluation - "not the time/place for"
 * - Different from simple ではない (copula negation) - requires どころ
 * - Different from どころか (far from, on the contrary) - different meaning
 * - The どころ attaches to the preceding word/phrase
 * - Followed by ではない/じゃない/ではありません/etc.
 *
 * GiNZA parse structure:
 * - 痛い(ADJ) + どころ(ADP/PART) + で(AUX,lemma=だ) + は(ADP,fixed) + ない(AUX,fixed)
 * - 取る(VERB) + どころ(PART) + じゃ(AUX,lemma=だ) + ない(AUX,fixed)
 * - マラソン(NOUN) + どころ(ADP) + で(AUX) + は(fixed) + あり(VERB,fixed) + ませ(AUX,fixed)
 * - お出掛け(NOUN) + どころ(ADP) + で(AUX,cop) + は(fixed) + なくなっ(VERB)
 *
 * Different from:
 * - どころか (far from, on the contrary - JLPT3) - expresses contrast, not impossibility
 * - Simple ではない negation - lacks the emphatic どころ
 * - として (toshite) - "as", different pattern
 * - にしては (nishite) - "considering", different pattern
 */
export default bunproLinguisticRule('どころではない', (r) => {
  // Match the どころ token (lemma=どころ, pos=ADP or PART)
  const dokoro = r.tok({
    lemma: 'どころ',
    posOneOf: ['ADP', 'PART'],
  }, 'dokoro');

  r.either(
    // Pattern 1a: どころではない (with lemma=だ)
    (b1) => {
      const de = b1.aux({
        lemma: 'だ',
        text: 'で',
      }, 'de');
      const wa = b1.tok({
        text: 'は',
        dep: 'fixed',
      }, 'wa');
      const nai = b1.aux({
        lemma: 'ない',
        dep: 'fixed',
      }, 'nai');

      b1.inOrder(dokoro, de, 1);
      b1.inOrder(de, wa, 1);
      b1.inOrder(wa, nai, 1);

      b1.captureSpan('どころではない', dokoro, nai);
    },

    // Pattern 1b: どころではない (with lemma=で - GiNZA inconsistency)
    (b1b) => {
      const de = b1b.aux({
        text: 'で',
      }, 'de');
      const wa = b1b.tok({
        text: 'は',
        dep: 'fixed',
      }, 'wa');
      const nai = b1b.aux({
        lemma: 'ない',
        dep: 'fixed',
      }, 'nai');

      b1b.inOrder(dokoro, de, 1);
      b1b.inOrder(de, wa, 1);
      b1b.inOrder(wa, nai, 1);

      b1b.captureSpan('どころではない', dokoro, nai);
    },

    // Pattern 2: どころじゃない (casual/standard)
    (b2) => {
      const ja = b2.aux({
        lemma: 'だ',
        text: 'じゃ',
      }, 'ja');
      const nai = b2.aux({
        lemma: 'ない',
        dep: 'fixed',
      }, 'nai');

      b2.inOrder(dokoro, ja, 1);
      b2.inOrder(ja, nai, 1);

      b2.captureSpan('どころではない', dokoro, nai);
    },

    // Pattern 3: どころではありません (polite)
    (b3) => {
      const de = b3.aux({
        lemma: 'だ',
        text: 'で',
      }, 'de');
      const wa = b3.tok({
        text: 'は',
        dep: 'fixed',
      }, 'wa');
      const aru = b3.verb({
        lemma: 'ある',
        dep: 'fixed',
      }, 'aru');
      const masu = b3.aux({
        lemma: 'ます',
        dep: 'fixed',
      }, 'masu');

      b3.inOrder(dokoro, de, 1);
      b3.inOrder(de, wa, 1);
      b3.inOrder(wa, aru, 1);
      b3.inOrder(aru, masu, 1);

      b3.captureSpan('どころではない', dokoro, masu);
    },

    // Pattern 4: どころじゃありません (polite)
    (b4) => {
      const ja = b4.aux({
        lemma: 'だ',
        text: 'じゃ',
      }, 'ja');
      const aru = b4.verb({
        lemma: 'ある',
        dep: 'fixed',
      }, 'aru');
      const masu = b4.aux({
        lemma: 'ます',
        dep: 'fixed',
      }, 'masu');

      b4.inOrder(dokoro, ja, 1);
      b4.inOrder(ja, aru, 1);
      b4.inOrder(aru, masu, 1);

      b4.captureSpan('どころではない', dokoro, masu);
    },

    // Pattern 5: どころではないです (polite with desu)
    (b5) => {
      const de = b5.aux({
        lemma: 'だ',
        text: 'で',
      }, 'de');
      const wa = b5.tok({
        text: 'は',
        dep: 'fixed',
      }, 'wa');
      const nai = b5.aux({
        lemma: 'ない',
        dep: 'fixed',
      }, 'nai');
      const desu = b5.aux({
        lemma: 'だ',
        text: 'です',
        dep: 'fixed',
      }, 'desu');

      b5.inOrder(dokoro, de, 1);
      b5.inOrder(de, wa, 1);
      b5.inOrder(wa, nai, 1);
      b5.inOrder(nai, desu, 1);

      b5.captureSpan('どころではない', dokoro, desu);
    },

    // Pattern 6: どころじゃないです (polite with desu)
    (b6) => {
      const ja = b6.aux({
        lemma: 'だ',
        text: 'じゃ',
      }, 'ja');
      const nai = b6.aux({
        lemma: 'ない',
        dep: 'fixed',
      }, 'nai');
      const desu = b6.aux({
        lemma: 'だ',
        text: 'です',
        dep: 'fixed',
      }, 'desu');

      b6.inOrder(dokoro, ja, 1);
      b6.inOrder(ja, nai, 1);
      b6.inOrder(nai, desu, 1);

      b6.captureSpan('どころではない', dokoro, desu);
    },

    // Pattern 7: どころではなくなった (past tense - became out of the question)
    (b7) => {
      const de = b7.aux({
        lemma: 'だ',
        text: 'で',
      }, 'de');
      const wa = b7.tok({
        text: 'は',
        dep: 'fixed',
      }, 'wa');
      const nakunatt = b7.verb({
        lemma: 'なくなる',
      }, 'nakunatt');

      b7.inOrder(dokoro, de, 1);
      b7.inOrder(de, wa, 1);
      b7.inOrder(wa, nakunatt, 3);

      b7.captureSpan('どころではない', dokoro, nakunatt);
    },

    // Pattern 8: どころじゃなくなった (casual past tense)
    (b8) => {
      const ja = b8.aux({
        lemma: 'だ',
        text: 'じゃ',
      }, 'ja');
      const nakunatt = b8.verb({
        lemma: 'なくなる',
      }, 'nakunatt');

      b8.inOrder(dokoro, ja, 1);
      b8.inOrder(ja, nakunatt, 3);

      b8.captureSpan('どころではない', dokoro, nakunatt);
    }
  );
});
