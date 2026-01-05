import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: よていだ (よていだ) - Plan to, Intend to
 *
 * Matches Verb/[noun] + 予定 + だ/です/だった to express plans or schedules.
 *
 * Structures:
 * - Verb + 予定 + だ/です/だった (plain form verb + plan)
 * - Noun + の + 予定 + だ/です/だった (noun + plan)
 *
 * Examples:
 * - 勉強し始める予定です (plan to start studying)
 * - 喋れるようになる予定だ (plan to become able to speak)
 * - 月曜日の予定です (planned for Monday)
 * - 会議は月曜日の予定です (meeting is planned for Monday)
 *
 * Key discriminators:
 * - 予定 is a NOUN (not a verb or auxiliary)
 * - Must be followed by copula (だ/です/だった)
 * - Attaches to verbs directly (no particle)
 * - Attaches to nouns with の particle
 *
 * GiNZA parse structure:
 * - 予定: NOUN with lemma=予定
 * - だ/です/だった: AUX with lemma=だ/です/だった (copula)
 * - For verbs: 予定 attaches directly with dep=root or similar
 * - For nouns: の particle connects noun to 予定
 *
 * Note: Standalone 予定 (without copula) is NOT matched
 * to avoid false positives and ensure we match the full grammar pattern.
 */
export default bunproLinguisticRule('よていだ', (r) => {
  r.either(
    // Branch 1: よてい + だ (casual, present) - after verb or noun+の
    (b) => {
      const yotei = b.tok({ lemma: 'よてい', pos: 'NOUN' }, 'yotei');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.inOrder(yotei, da, 1);
      b.captureSpan('よていだ', yotei, da);
    },

    // Branch 2: よてい + です (polite, present) - after verb or noun+の
    (b) => {
      const yotei = b.tok({ lemma: 'よてい', pos: 'NOUN' }, 'yotei');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.inOrder(yotei, desu, 1);
      b.captureSpan('よていだ', yotei, desu);
    },

    // Branch 3: よてい + だった (casual, past) - after verb or noun+の
    (b) => {
      const yotei = b.tok({ lemma: 'よてい', pos: 'NOUN' }, 'yotei');
      const datta = b.aux({ lemma: 'だった' }, 'datta');
      b.inOrder(yotei, datta, 1);
      b.captureSpan('よていだ', yotei, datta);
    },

    // Branch 4: の + よてい + だ (noun + casual, present)
    (b) => {
      const no = b.particle('の', 'no');
      const yotei = b.tok({ lemma: 'よてい', pos: 'NOUN' }, 'yotei');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.inOrder(no, yotei, 1);
      b.inOrder(yotei, da, 1);
      b.captureSpan('よていだ', no, da);
    },

    // Branch 5: の + よてい + です (noun + polite, present)
    (b) => {
      const no = b.particle('の', 'no');
      const yotei = b.tok({ lemma: 'よてい', pos: 'NOUN' }, 'yotei');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.inOrder(no, yotei, 1);
      b.inOrder(yotei, desu, 1);
      b.captureSpan('よていだ', no, desu);
    },

    // Branch 6: の + よてい + だった (noun + casual, past)
    (b) => {
      const no = b.particle('の', 'no');
      const yotei = b.tok({ lemma: 'よてい', pos: 'NOUN' }, 'yotei');
      const datta = b.aux({ lemma: 'だった' }, 'datta');
      b.inOrder(no, yotei, 1);
      b.inOrder(yotei, datta, 1);
      b.captureSpan('よていだ', no, datta);
    }
  );
});
