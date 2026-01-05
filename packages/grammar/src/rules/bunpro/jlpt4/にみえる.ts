import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: にみえる (to look like / to seem / to appear)
 *
 * Matches various patterns where something appears/looks a certain way based on
 * visual impression. The particle に marks the state/condition being perceived.
 *
 * This grammar point specifically refers to visual appearance - how something
 * looks to the observer. It's more objective than subjective conjecture.
 *
 * Structure:
 * - Noun + に + みえる (e.g., アイスクリームにみえる - looks like ice cream)
 * - Noun + の + よう + に + みえる (e.g., 家族のようにみえる - looks like family)
 * - [い]Adjective (く-form) + に + みえる (e.g., やすくみえる - looks cheap)
 * - [な]Adjective + に + みえる (e.g., 綺麗にみえる - looks clean)
 * - Auxiliary そう + に + みえる (e.g., 忙しそうにみえる - looks busy)
 * - Verb + よう + に + みえる (e.g., 怒っているようにみえる - looks angry)
 *
 * This is different from:
 * - とみえる - objective conclusion from evidence
 * - ようだ - subjective conjecture (not specifically visual)
 * - そうだ - hearsay or impression
 * - らしい - hearsay or general impression
 */
export default bunproLinguisticRule('にみえる', (r) => {
  r.either(
    // Pattern 1: Noun + に + みえる (direct visual appearance)
    // e.g., アイスクリームにみえる, 雲にみえる, ニキビ
    (b1) => {
      const ni = b1.particle('に', 'ni');
      const mieru = b1.tok({
        text: 'みえる',
        pos: 'VERB',
      }, 'mieru');

      b1.inOrder(ni, mieru, 1);
      b1.captureSpan('にみえる', ni, mieru);
    },

    // Pattern 2: Noun + の + よう + に + みえる (comparison + appearance)
    // e.g., 顔のようにみえる, 家族のようにみえる
    (b2) => {
      const no = b2.particle('の', 'no');
      const you = b2.tok({
        text: 'よう',
      }, 'you');
      const ni = b2.tok({
        text: 'に',
      }, 'ni');
      const mieru = b2.tok({
        text: 'みえる',
        pos: 'VERB',
      }, 'mieru');

      b2.inOrder(no, you, 1);
      b2.inOrder(you, ni, 1);
      b2.inOrder(ni, mieru, 1);
      b2.captureSpan('にみえる', no, mieru);
    },

    // Pattern 3: Verb/ Auxiliary + よう/みたい + に + みえる (state + appearance)
    // e.g., 溺れているようにみえる, 太ったみたいにみえる, 走っているようにみえる
    (b3) => {
      const you = b3.tok({
        textOneOf: ['よう', 'みたい'],
      }, 'you');
      const ni = b3.tok({
        text: 'に',
      }, 'ni');
      const mieru = b3.tok({
        text: 'みえる',
        pos: 'VERB',
      }, 'mieru');

      b3.inOrder(you, ni, 1);
      b3.inOrder(ni, mieru, 1);
      b3.captureSpan('にみえる', you, mieru);
    },

    // Pattern 4: い-adjective (く-form) + みえる (no に particle!)
    // Matches adjective/verb in renyoukei form + みえる
    // e.g., やすくみえる, 高くみえる, 軽くみえる, 暑くみえる
    // The く-form can appear as either:
    // - A single token (ADJ/VERB/NOUN) ending in く (renyoukei form)
    // - Any token + く (auxiliary/particle) + みえる
    (b4) => {
      const mieru = b4.tok({
        text: 'みえる',
        pos: 'VERB',
      }, 'mieru');

      b4.either(
        // Sub-pattern 4a: Token ending in く directly before みえる
        // Handles: やすく, 高く, 軽く, たかく, かるく, etc.
        (sub1) => {
          const kuForm = sub1.tok({
            textOneOf: ['やすく', '高く', 'たかく', '軽く', 'かるく', '暑く', 'おもしろく', 'たのしく'],
          }, 'kuForm');
          sub1.inOrder(kuForm, mieru, 1);
          sub1.captureSpan('にみえる', kuForm, mieru);
        },
        // Sub-pattern 4b: Any token + く (as particle/aux) + みえる
        // e.g., where く is analyzed separately
        (sub2) => {
          const ku = sub2.tok({
            text: 'く',
          }, 'ku');
          sub2.inOrder(ku, mieru, 1);
          sub2.captureSpan('にみえる', ku, mieru);
        },
        // Sub-pattern 4c: Generic ADJ token + みえる (catch-all)
        (sub3) => {
          const adj = sub3.adj({}, 'adj');
          sub3.inOrder(adj, mieru, 1);
          sub3.captureSpan('にみえる', adj, mieru);
        }
      );
    },

    // Pattern 5: な-adjective + に + みえる
    // e.g., 綺麗にみえる
    (b5) => {
      const ni = b5.particle('に', 'ni');
      const mieru = b5.tok({
        text: 'みえる',
        pos: 'VERB',
      }, 'mieru');

      b5.inOrder(ni, mieru, 1);
      b5.captureSpan('にみえる', ni, mieru);
    },

    // Pattern 6: そう (auxiliary) + に + みえる
    // e.g., 寒そうにみえる, 忙しそうにみえる, 幸せそうにみえる
    (b6) => {
      const sou = b6.tok({
        text: 'そう',
      }, 'sou');
      const ni = b6.tok({
        text: 'に',
      }, 'ni');
      const mieru = b6.tok({
        text: 'みえる',
        pos: 'VERB',
      }, 'mieru');

      b6.inOrder(sou, ni, 1);
      b6.inOrder(ni, mieru, 1);
      b6.captureSpan('にみえる', sou, mieru);
    }
  );
});
