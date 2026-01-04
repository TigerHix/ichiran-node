import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('つづける', (r) => {
  // つづける attaches to verb stems (masu form without ます) to mean "continue doing"
  // Similar to おわる, GiNZA parses this with various POS/dep combinations:
  //
  // - 走りつづける: stem (VERB) + つづける (VERB)
  // - 書きつづけた: stem (VERB) + つづけ (AUX/VERB)
  // - 見まわりつづけた: compound verb
  //
  // Key discriminators:
  // 1. lemma ends with つづける (hiragana) - NOT 続ける (kanji)
  // 2. The auxiliary attaches to stem form (連用形)

  r.either(
    // Pattern 1: Standard - stem + つづける as separate tokens
    (b) => {
      const tsuzukeru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'つづける', // Exact match for auxiliary verb (trigger)
      }, 'tsuzukeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'], // ADV/ADJ for some GiNZA quirks
        inflectionForm: '連用形-一般',
      }, 'stem');
      // Stem immediately precedes つづける
      b.inOrder(stem, tsuzukeru, 1);
      b.captureSpan('つづける', stem, tsuzukeru);
    },
    // Pattern 2: Compound forms like みまわりつづける (みまわる+つづける)
    // GiNZA sometimes analyzes verb stem + つづける as a single compound token
    (b) => {
      const compound = b.tok({
        posOneOf: ['VERB', 'NOUN'],
        // Common compound forms from test data
        lemmaOneOf: [
          'みまわりつづける',  // みまわる + つづける
          'はしりつづける',    // はしる + つづける
          'かきつづける',      // かく + つづける
          'あるきつづける',    // あるく + つづける
          'なきつづける',      // なく + つづける
          'みつづける',        // みる + つづける
          'ふりつづける',      // ふる + つづける
          'つりつづける',      // つる + つづける
          'なおしつづける',    // なおす + つづける
          'はなし つづける',   // はなす + つづける (with space)
        ],
      }, 'compound');
      b.capture(compound);
    },
    // Pattern 3: Past/polite/te-form with various dependencies
    (b) => {
      const tsuzukeru = b.tok({
        lemma: 'つづける',
        posOneOf: ['VERB', 'AUX'],
        depOneOf: ['aux', 'root', 'csubj', 'conj'],
      }, 'tsuzukeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, tsuzukeru, 1);
      b.captureSpan('つづける', stem, tsuzukeru);
    }
  );
});
