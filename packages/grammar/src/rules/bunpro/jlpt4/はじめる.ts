import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('はじめる', (r) => {
  // はじめる attaches to verb stems (masu form without ます) to mean "start doing"
  // Similar to おわる and つづける, GiNZA parses this with various POS/dep combinations:
  //
  // - 降りはじめる: stem (VERB) + はじめる (VERB)
  // - 歌いはじめた: stem (VERB) + はじめ (AUX/VERB)
  // - 働きはじめました: stem (VERB) + はじめ (AUX)
  // - しおわった: compound "しおわっ" (VERB/NOUN, lemma=しおわる)
  //
  // Key discriminators:
  // 1. lemma is はじめる (hiragana) - NOT 始める (kanji)
  // 2. The auxiliary attaches to stem form (連用形)

  r.either(
    // Pattern 1: Standard - stem + はじめる as separate tokens
    // Try with inflectionForm constraint first
    (b) => {
      const hajimeru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'はじめる',
      }, 'hajimeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, hajimeru, 1);
      b.captureSpan('はじめる', stem, hajimeru);
    },
    // Pattern 2: Without inflectionForm constraint (GiNZA inconsistency)
    (b) => {
      const hajimeru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'はじめる',
      }, 'hajimeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ', 'NOUN'],
      }, 'stem');
      b.inOrder(stem, hajimeru, 1);
      b.captureSpan('はじめる', stem, hajimeru);
    },
    // Pattern 2b: Special case for suru-verbs (する + はじめる)
    // GiNZA may parse "し" as the stem with lemma=する
    (b) => {
      const hajimeru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'はじめる',
      }, 'hajimeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['する'],
      }, 'stem');
      b.inOrder(stem, hajimeru, 1);
      b.captureSpan('はじめる', stem, hajimeru);
    },
    // Pattern 3: Conjugated forms (past, conditional, etc.)
    // Match stem + auxiliary where auxiliary has lemma=はじめる
    // but text is conjugated (はじめ, はじめた, etc.)
    (b) => {
      const aux = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'はじめる',
        // Text contains はじめ pattern (may be conjugated)
        textOneOf: [
          'はじめ',      // stem form
          'はじめる',    // present
          'はじめて',    // te-form
          'はじめた',    // past
          'はじめます',  // polite present
          'はじめました', // polite past
        ],
      }, 'aux');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ', 'NOUN'],
      }, 'stem');
      b.inOrder(stem, aux, 1);
      b.captureSpan('はじめる', stem, aux);
    },
    // Pattern 4: Compound forms where GiNZA creates single compound token
    (b) => {
      const compound = b.tok({
        posOneOf: ['VERB', 'NOUN'],
        lemmaOneOf: [
          'ふりはじめる',
          'うたいはじめる',
          'ためはじめる',
          'さわぎはじめる',
          'はたらきはじめる',
          'はなしはじめる',
          'およぎはじめる',
          'いきはじめる',
          'ならいはじめる',
          'しおわる',  // Some sentences use compound form
          'なきはじめる',
          'よみはじめる',
          'かきはじめる',
          'あるきはじめる',
        ],
      }, 'compound');
      b.capture(compound);
    }
  );
});
