import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: 風 (ふう) - style, manner, way of doing
 *
 * Noun + 風 = in the style of, -style, in the manner of
 * Examples:
 * - アメリカ風料理 (American-style food)
 * - 関西風お好み焼き (Kansai-style okonomiyaki)
 * - 漫画風 (manga-style)
 * - １９世紀風時計 (19th-century-style clock)
 *
 * Key patterns:
 * 1. Noun + 風 (standard suffix pattern)
 * 2. Noun + 風 + の + Noun (when followed by particle の)
 *
 * GiNZA parse structure:
 * - When 風 is written in hiragana (ふう), GiNZA may parse it as NOUN or PROPN
 * - The lemma can be "ふう" or "風"
 *
 * Strategy: Match any noun (or proper noun) with text="ふう" or text="風"
 * that follows another token (the base noun).
 *
 * Note: 風 is read as "ふう" in this grammar point, not "かぜ" (wind).
 */
export default bunproLinguisticRule('風', (r) => {
  r.either(
    // Pattern 1: Noun + ふう (hiragana form - most common in tests)
    (b) => {
      const fuu = b.tok({
        text: 'ふう',
      }, 'fuu');

      const prev = b.tok({}, 'noun');
      b.inOrder(prev, fuu, 1);
      b.captureSpan('風', prev, fuu);
    },

    // Pattern 2: Noun + 風 (kanji form)
    (b) => {
      const fuu = b.tok({
        text: '風',
        lemma: 'ふう',
      }, 'fuu');

      const prev = b.tok({}, 'noun');
      b.inOrder(prev, fuu, 1);
      b.captureSpan('風', prev, fuu);
    },

    // Pattern 3: Noun + ふう + の (with particle)
    (b) => {
      const fuu = b.tok({
        text: 'ふう',
      }, 'fuu');

      const no = b.particle('の', 'no');
      b.inOrder(fuu, no, 1);

      const prev = b.tok({}, 'noun');
      b.inOrder(prev, fuu, 1);
      b.captureSpan('風', prev, fuu);
    },

    // Pattern 4: Noun + 風 + の (with particle, kanji form)
    (b) => {
      const fuu = b.tok({
        text: '風',
        lemma: 'ふう',
      }, 'fuu');

      const no = b.particle('の', 'no');
      b.inOrder(fuu, no, 1);

      const prev = b.tok({}, 'noun');
      b.inOrder(prev, fuu, 1);
      b.captureSpan('風', prev, fuu);
    }
  );
});
