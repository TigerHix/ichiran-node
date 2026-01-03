import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: する (suru) - the basic "to do" verb
 *
 * する is a highly irregular verb used for many different actions.
 * It's one of only two main irregular verbs in Japanese (along with くる).
 *
 * Forms handled:
 * - Dictionary form: する
 * - Polite form: します
 * - Negative: しない
 * - Past: した
 * - Past polite: しました
 * - Negative past: しなかった
 *
 * Note: This rule matches standalone する, NOT suru-verb compounds
 * like 勉強する, サッカーする, etc. (those are separate dictionary entries).
 */
export default linguisticRule('する', (r) => {
  r.either(
    // Dictionary form: する
    (b) => {
      const suru = b.verb({
        lemma: 'する',
        conjugationClass: 'サ行変格',
      }, 'suru');
      b.capture(suru);
    },

    // Polite form: します
    (b) => {
      const shi = b.aux({
        lemma: 'する',
      }, 'shi');
      const masu = b.aux({
        lemma: 'ます',
      }, 'masu');
      b.auxOf(shi, masu);
      b.captureSpan('する', shi, masu);
    },

    // Negative: しない
    (b) => {
      const shinai = b.verb({
        text: 'しない',
        lemma: 'する',
      }, 'shinai');
      b.capture(shinai);
    },

    // Polite negative: しません
    (b) => {
      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '未然形-一般',
      }, 'shi');
      const masen = b.aux({
        lemma: 'ます',
      }, 'masen');
      b.auxOf(shi, masen);
      b.captureSpan('する', shi, masen);
    },

    // Past: した
    (b) => {
      const shita = b.verb({
        text: 'した',
        lemma: 'する',
      }, 'shita');
      b.capture(shita);
    },

    // Past polite: しました
    (b) => {
      const shi = b.aux({
        lemma: 'する',
        inflectionForm: '連用形-一般',
      }, 'shi');
      const mashita = b.aux({
        lemma: 'ます',
      }, 'mashita');
      b.auxOf(shi, mashita);
      b.captureSpan('する', shi, mashita);
    },

    // Negative past: しなかった
    (b) => {
      const shinakatta = b.verb({
        text: 'しなかった',
        lemma: 'する',
      }, 'shinakatta');
      b.capture(shinakatta);
    }
  );
});
