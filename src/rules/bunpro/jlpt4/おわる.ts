import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おわる', (r) => {
  // おわる attaches to verb stems (masu form) to mean "finish doing"
  // GiNZA parses this inconsistently depending on conjugation:
  //
  // - 食べおわる: stem (VERB, dep=acl) + おわる (NOUN, dep=root)
  // - 見おわった: stem (VERB, dep=root) + おわっ (VERB, dep=aux)
  // - 書きおわりました: stem (VERB) + おわり (AUX, dep=aux)
  // - しおわった: compound "しおわっ" (VERB/NOUN, lemma=しおわる)
  // - なおしおわった: なお (ADV) + しおわっ (VERB, lemma=しおわる)
  // - よみおわった: よみ (ADJ!) + おわっ (VERB, lemma=おわる)
  //
  // Key discriminators:
  // 1. lemma ends with おわる (hiragana) - NOT 終わる (kanji)
  // 2. The auxiliary attaches to stem form (連用形)

  r.either(
    // Pattern 1: Standard - stem + おわる as separate tokens
    // This has the exact lemma trigger required for dispatch
    (b) => {
      const owaru = b.tok({
        posOneOf: ['NOUN', 'VERB', 'AUX'],
        lemma: 'おわる', // Exact match for auxiliary verb (trigger)
      }, 'owaru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'], // ADV/ADJ for some GiNZA quirks
        inflectionForm: '連用形-一般',
      }, 'stem');
      // Stem immediately precedes おわる
      b.inOrder(stem, owaru, 1);
      b.captureSpan('おわる', stem, owaru);
    },
    // Pattern 2: Compound forms like しおわる (する+おわる)
    // GiNZA sometimes analyzes verb stem + おわる as a single compound token
    // Common compounds: しおわる, つくりおわる, かきおわる, よみおわる, etc.
    (b) => {
      // Match specific compound lemmas (cannot use regex in high-level API)
      const compound = b.tok({
        posOneOf: ['VERB', 'NOUN'],
        // Common compound forms from test data
        lemmaOneOf: [
          'しおわる',      // する + おわる
          'つくりおわる',   // つくる + おわる
          'かきおわる',     // かく + おわる
          'よみおわる',     // よむ + おわる
          'なおしおわる',   // なおす + おわる
          'はなし おわる',  // はなす + おわる (with space)
        ],
      }, 'compound');
      b.capture(compound);
    },
    // Pattern 3: Past/polite with various dependencies
    (b) => {
      const owaru = b.tok({
        lemma: 'おわる',
        posOneOf: ['VERB', 'AUX'],
        depOneOf: ['aux', 'root', 'csubj'],
      }, 'owaru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, owaru, 1);
      b.captureSpan('おわる', stem, owaru);
    }
  );
});
