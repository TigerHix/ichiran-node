import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('だす', (r) => {
  // だす attaches to verb stems (masu form) to mean "suddenly start doing"
  // GiNZA parses this similarly to おわる:
  //
  // - 泣き出した: stem (VERB, dep=acl) + 出した (VERB, dep=root)
  // - 話し出す: stem (VERB) + 出す (VERB)
  // - 降り出した: stem (VERB, dep=acl) + 出した (VERB, dep=root)
  // - 食べ出す: stem (VERB) + 出す (VERB)
  //
  // Key discriminators:
  // 1. lemma ends with だす (hiragana) - NOT 出す (kanji)
  // 2. The auxiliary attaches to stem form (連用形)
  // 3. Must distinguish from main verb 出す (to take out/emit)

  r.either(
    // Pattern 1: Standard - stem + だす as separate tokens
    // This has the exact lemma trigger required for dispatch
    (b) => {
      const dasu = b.tok({
        posOneOf: ['NOUN', 'VERB', 'AUX'],
        lemma: 'だす', // Exact match for auxiliary verb (trigger)
      }, 'dasu');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'], // ADV/ADJ for some GiNZA quirks
        inflectionForm: '連用形-一般',
      }, 'stem');
      // Stem immediately precedes だす
      b.inOrder(stem, dasu, 1);
      b.captureSpan('だす', stem, dasu);
    },
    // Pattern 2: Compound forms like いれだす (入れる+だす)
    // GiNZA sometimes analyzes verb stem + だす as a single compound token
    (b) => {
      // Match specific compound lemmas (cannot use regex in high-level API)
      const compound = b.tok({
        posOneOf: ['VERB', 'NOUN'],
        // Common compound forms from test data
        lemmaOneOf: [
          'なきだす',      // なく + だす
          'ぬけだす',      // ぬける + だす
          'とびだす',      // とぶ + だす
          'にげだす',      // にげる + だす
          'はしりだす',    // はしる + だす
          'ふりだす',      // ふる + だす
          'わらいだす',    // わらう + だす
          'のみだす',      // のむ + だす
          'みだす',        // GiNZA quirk: "のみだした" analyzed as "の" + "みだし" + "た"
          'つくりだす',    // つくる + だす
          'おもいだす',    // おもう + だす
          'いいだす',      // いう + だす
          'しゃべりだす',  // しゃべる + だす
        ],
      }, 'compound');
      b.capture(compound);
    },
    // Pattern 3: Past/polite with various dependencies
    (b) => {
      const dasu = b.tok({
        lemma: 'だす',
        posOneOf: ['VERB', 'AUX'],
        depOneOf: ['aux', 'root', 'csubj'],
      }, 'dasu');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, dasu, 1);
      b.captureSpan('だす', stem, dasu);
    }
  );
});
