import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: がひつよう (need, necessary)
 *
 * Pattern: Noun + が必要 + (copula)
 *
 * Indicates that a noun or noun-like expression is necessary/needed.
 *
 * Structures:
 * - Casual: Noun + が必要だ / が必要だった / が必要だろう
 * - Polite: Noun + が必要です / が必要でした / 必要でしょう
 *
 * Examples:
 * - 免許が必要だ (A license is necessary)
 * - コーヒーが必要です (Coffee is necessary)
 * - 時間があった (There was time)
 *
 * Note: が must be the subject marker (dep=nsubj), not topic marker (dep=topic).
 * GiNZA parses が as particle with dep=nsubj when marking subjects.
 *
 * The test data uses hiragana ひつよう, but real Japanese text uses kanji 必要.
 * Both forms should match.
 */
export default bunproLinguisticRule('がひつよう', (r) => {
  const ga = r.particle('が', 'ga');
  // Match both kanji (必要) and hiragana (ひつよう) forms
  // GiNZA parses these inconsistently: sometimes NOUN, sometimes ADJ, sometimes VERB
  const hitsuyou = r.tok({
    posOneOf: ['NOUN', 'ADJ', 'VERB'],
    textOneOf: ['必要', 'ひつよう']
  }, 'hitsuyou');

  r.either(
    // Pattern 1: Casual present - が必要だ
    (b) => {
      const da = b.aux({ text: 'だ', lemma: 'だ' }, 'da');
      b.inOrder(ga, hitsuyou, 1);
      b.inOrder(hitsuyou, da, 1);
      b.captureSpan('が必要だ', ga, da);
    },
    // Pattern 2: Polite present - が必要です
    (b) => {
      const desu = b.aux({ text: 'です', lemma: 'です' }, 'desu');
      b.inOrder(ga, hitsuyou, 1);
      b.inOrder(hitsuyou, desu, 1);
      b.captureSpan('が必要です', ga, desu);
    },
    // Pattern 3: Casual past - が必要だった
    (b) => {
      const dat = b.aux({ text: 'だっ', lemma: 'だ' }, 'dat');
      const ta = b.aux({ text: 'た', lemma: 'た' }, 'ta');
      b.inOrder(ga, hitsuyou, 1);
      b.inOrder(hitsuyou, dat, 1);
      b.inOrder(dat, ta, 1);
      b.captureSpan('が必要だった', ga, ta);
    },
    // Pattern 4: Polite past - が必要でした
    (b) => {
      const deshi = b.aux({ text: 'でし', lemma: 'です' }, 'deshi');
      const ta = b.aux({ text: 'た', lemma: 'た' }, 'ta');
      b.inOrder(ga, hitsuyou, 1);
      b.inOrder(hitsuyou, deshi, 1);
      b.inOrder(deshi, ta, 1);
      b.captureSpan('が必要でした', ga, ta);
    },
    // Pattern 5: Casual conjecture - が必要だろう
    (b) => {
      const darou = b.aux({ text: 'だろう', lemma: 'だろう' }, 'darou');
      b.inOrder(ga, hitsuyou, 1);
      b.inOrder(hitsuyou, darou, 1);
      b.captureSpan('が必要だろう', ga, darou);
    },
    // Pattern 6: Polite conjecture - が必要でしょう
    (b) => {
      const deshou = b.aux({ text: 'でしょう', lemma: 'でしょう' }, 'deshou');
      b.inOrder(ga, hitsuyou, 1);
      b.inOrder(hitsuyou, deshou, 1);
      b.captureSpan('が必要でしょう', ga, deshou);
    },
    // Pattern 7: No copula (casual speech) - が + 必要 alone
    (b) => {
      b.inOrder(ga, hitsuyou, 1);
      b.captureSpan('が必要', ga, hitsuyou);
    }
  );
});
