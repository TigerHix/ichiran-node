import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: という風に (to iu fuu ni) - As if to say, In the manner of
 *
 * A structure meaning that something is being said in the "way" or "wind" of (A).
 * Expresses doing something "as if to say (A)" or "in such a way that (A)".
 *
 * Structure: Phrase + という + 風(ふう) + に
 *
 * The pattern combines:
 * - という (to iu) - quoting/quotative particle
 * - 風/ふう (fuu) - manner, way, appearance (noun)
 * - に (ni) - particle marking manner/method
 *
 * Examples:
 * - 彼女は昼休憩に帰るというふうに聞いています。
 *   (I heard something as if to suggest that she is going to go home during lunch break.)
 * - 鈴木さんはこの試験に命がかかっているというふうに勉強をした。
 *   (Suzuki-san studied as if to suggest that his life depended on this exam.)
 * - 彼はデザイン、僕はプログラミング、というふうに役割を分けてアプリを開発することにした。
 *   (We decided to develop the application by dividing the roles in the following way...)
 * - 「これは仮定の話です」という風に言われても、あまりにもリアルすぎる。
 *   (Even if I was told in such a way that suggested 'this is just a hypothetical conversation'...)
 *
 * Key discriminators:
 * - という is the quotative particle combination
 * - 風/ふう is a noun meaning "manner, way, appearance"
 * - に is the particle marking method/manner
 * - Both 風 and ふう variants should match
 * - Can follow quotes, verbs, or nouns
 *
 * GiNZA parse structure:
 * - という is typically ADP + AUX (quotative)
 * - 風/ふう is NOUN
 * - に is ADP/Particle
 *
 * Different from:
 * - 単に風に (just + wind + to) - literal wind
 * - というように (to iu you ni) - more direct comparison (ように)
 * - かのようだ (ka no you da) - more imaginative/unreal
 */
export default linguisticRule('という風に', (r) => {
  r.either(
    // Pattern 1: という + 風 + に (kanji form)
    // Most common: quotation + という + 風 + に
    (b1) => {
      const to = b1.tok({ text: 'と', pos: 'ADP' }, 'to');
      const iu = b1.aux({ lemma: 'いう' }, 'iu');
      const fuu = b1.noun({ lemma: '風' }, 'fuu');
      const ni = b1.particle('に', 'ni');

      b1.inOrder(to, iu, 1);
      b1.inOrder(iu, fuu, 1);
      b1.inOrder(fuu, ni, 1);

      b1.captureSpan('という風に', to, ni);
    },

    // Pattern 2: という + ふう + に (hiragana form)
    // Alternative writing with hiragana ふう
    (b2) => {
      const to = b2.tok({ text: 'と', pos: 'ADP' }, 'to');
      const iu = b2.aux({ lemma: 'いう' }, 'iu');
      const fuu = b2.noun({ text: 'ふう', lemma: '風' }, 'fuu');
      const ni = b2.particle('に', 'ni');

      b2.inOrder(to, iu, 1);
      b2.inOrder(iu, fuu, 1);
      b2.inOrder(fuu, ni, 1);

      b2.captureSpan('という風に', to, ni);
    },

    // Pattern 3: という (quoted form) + 風/ふう + に
    // When という is part of a fixed quotation expression
    (b3) => {
      const toiu = b3.tok({ text: 'という' }, 'toiu');
      const fuu = b3.noun({ lemmaOneOf: ['風', 'ふう'] }, 'fuu');
      const ni = b3.particle('に', 'ni');

      b3.inOrder(toiu, fuu, 1);
      b3.inOrder(fuu, ni, 1);

      b3.captureSpan('という風に', toiu, ni);
    },

    // Pattern 4: Loose pattern - catch-all for variant parsings
    // Allow any POS for 風/ふう to handle GiNZA variations
    (b4) => {
      const to = b4.tok({ text: 'と' }, 'to');
      const iu = b4.tok({ lemma: 'いう' }, 'iu');
      const fuu = b4.tok({ lemmaOneOf: ['風', 'ふう'] }, 'fuu');
      const ni = b4.particle('に', 'ni');

      b4.inOrder(to, iu, 2);
      b4.inOrder(iu, fuu, 2);
      b4.inOrder(fuu, ni, 1);

      b4.captureSpan('という風に', to, ni);
    }
  );
});
