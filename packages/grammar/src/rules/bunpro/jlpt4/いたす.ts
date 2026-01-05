import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: いたす (itasu) - humble form of する (suru)
 *
 * いたす is the humble form of する, used when speaking respectfully about
 * one's own actions or the actions of one's in-group (e.g., company).
 *
 * It's more humble than お〜する.
 *
 * Forms handled:
 * - Dictionary form: いたす
 * - Polite form: いたします (most common)
 * - Conjugated: いたしました, いたして, etc.
 * - Humble constructions: お+verb-stem+いたす, ご+suru-verb+いたす
 */
export default bunproLinguisticRule('いたす', (r) => {
  r.either(
    // Pattern 1: Simple いたす (dictionary form)
    (b) => {
      const itasu = b.verb({
        lemma: 'いたす',
      }, 'itasu');
      b.capture(itasu);
    },

    // Pattern 2: Polite いたします (most common form)
    // Also matches cases where します has lemma=いたす
    (b) => {
      const itashi = b.tok({
        lemma: 'いたす',
      }, 'itashi');
      const masu = b.tok({
        lemma: 'ます',
      }, 'masu');
      b.inOrder(itashi, masu, 2);
      b.captureSpan('いたす', itashi, masu);
    },

    // Pattern 2b: Polite いたします as single token (sometimes parsed this way)
    (b) => {
      const itashimasu = b.verb({
        text: 'いたします',
        lemma: 'いたす',
      }, 'itashimasu');
      b.capture(itashimasu);
    },

    // Pattern 3: Negative polite いたしません
    (b) => {
      const itashi = b.aux({
        lemma: 'いたす',
        inflectionForm: '未然形-一般',
      }, 'itashi');
      const masen = b.aux({
        lemma: 'ます',
      }, 'masen');
      b.auxOf(itashi, masen);
      b.captureSpan('いたす', itashi, masen);
    },

    // Pattern 4: Past polite いたしました
    (b) => {
      const itashi = b.aux({
        lemma: 'いたす',
        inflectionForm: '連用形-一般',
      }, 'itashi');
      const mashita = b.aux({
        lemma: 'ます',
      }, 'mashita');
      b.auxOf(itashi, mashita);
      b.captureSpan('いたす', itashi, mashita);
    },

    // Pattern 5: Te-form いたして (e.g., いたしましょうか)
    (b) => {
      const itashite = b.aux({
        lemma: 'いたす',
        inflectionForm: '連用形-一般',
      }, 'itashi');
      const te = b.aux({
        text: 'て',
        lemma: 'て',
      }, 'te');
      b.auxOf(itashite, te);
      b.captureSpan('いたす', itashite, te);
    },

    // Pattern 5b: Volitional いたしましょう (shall I do...?)
    (b) => {
      const itashi = b.aux({
        lemma: 'いたす',
      }, 'itashi');
      const mashou = b.aux({
        lemma: 'ます',
        inflectionForm: '意志推量形',
      }, 'mashou');
      b.auxOf(itashi, mashou);
      b.captureSpan('いたす', itashi, mashou);
    },

    // Pattern 6: Negative いたさない
    (b) => {
      const itasanai = b.verb({
        text: 'いたさない',
        lemma: 'いたす',
      }, 'itasanai');
      b.capture(itasanai);
    },

    // Pattern 7: Past いたした
    (b) => {
      const itashita = b.verb({
        text: 'いたした',
        lemma: 'いたす',
      }, 'itashita');
      b.capture(itashita);
    },

    // Pattern 8: お + verb-stem + いたします (humble construction, single token)
    // Example: お待ちいたします, お知らせいたします
    (b) => {
      const o = b.tok({
        text: 'お',
        pos: 'NOUN', // GiNZA parses お as NOUN in humble constructions
      }, 'o');

      const verbStem = b.tok({
        pos: 'NOUN', // Verb stem appears as NOUN in humble constructions
      }, 'verbStem');

      const itashimasu = b.verb({
        text: 'いたします',
        lemma: 'いたす',
      }, 'itashimasu');

      b.inOrder(o, verbStem, 1);
      b.inOrder(verbStem, itashimasu, 1);
      b.captureSpan('いたす', o, itashimasu);
    },

    // Pattern 8b: お + verb-stem + いたしましょう (humble volitional)
    // Example: お持ちいたしましょうか (shall I carry...?)
    (b) => {
      const o = b.tok({
        text: 'お',
        pos: 'NOUN',
      }, 'o');

      const verbStem = b.tok({
        pos: 'NOUN',
      }, 'verbStem');

      const itashite = b.aux({
        lemma: 'いたす',
        inflectionForm: '連用形-一般',
      }, 'itashite');

      const mashou = b.aux({
        lemma: 'ます',
        inflectionForm: '意志推量形',
      }, 'mashou');

      b.inOrder(o, verbStem, 1);
      b.inOrder(verbStem, itashite, 1);
      b.auxOf(itashite, mashou);
      b.captureSpan('いたす', o, mashou);
    },

    // Pattern 9: ご + suru-verb stem + いたします (humble suru-verb construction, single token)
    // Example: ご案内いたします, ご説明いたします
    (b) => {
      const go = b.tok({
        text: 'ご',
        pos: 'NOUN', // GiNZA parses ご as NOUN in humble constructions
      }, 'go');

      const noun = b.tok({
        pos: 'NOUN',
      }, 'noun');

      const itashimasu = b.verb({
        text: 'いたします',
        lemma: 'いたす',
      }, 'itashimasu');

      b.inOrder(go, noun, 1);
      b.inOrder(noun, itashimasu, 1);
      b.captureSpan('いたす', go, itashimasu);
    },

    // Pattern 10: お + verb-stem + いたす (dictionary form, polite)
    (b) => {
      const o = b.tok({
        text: 'お',
        pos: 'NOUN',
      }, 'o');

      const verbStem = b.tok({
        pos: 'NOUN',
      }, 'verbStem');

      const itasu = b.verb({
        lemma: 'いたす',
      }, 'itasu');

      b.inOrder(o, verbStem, 1);
      b.inOrder(verbStem, itasu, 1);
      b.captureSpan('いたす', o, itasu);
    },

    // Pattern 11: ご + suru-verb stem + いたす (dictionary form, polite)
    (b) => {
      const go = b.tok({
        text: 'ご',
        pos: 'NOUN',
      }, 'go');

      const noun = b.tok({
        pos: 'NOUN',
      }, 'noun');

      const itasu = b.verb({
        lemma: 'いたす',
      }, 'itasu');

      b.inOrder(go, noun, 1);
      b.inOrder(noun, itasu, 1);
      b.captureSpan('いたす', go, itasu);
    }
  );
});
