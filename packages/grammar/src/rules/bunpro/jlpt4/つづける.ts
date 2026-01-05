import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('つづける', (r) => {
  // つづける attaches to verb stems (masu form without ます) to mean "continue doing"
  // Similar to おわる, GiNZA parses this with various POS/dep combinations:
  //
  // - 走りつづける: stem (VERB) + つづける (VERB)
  // - 書きつづけた: stem (VERB) + つづけ (AUX/VERB)
  // - なきつづける: sometimes compound, sometimes separate
  // - しつづけたら: suru + tsuzukeru (conditional form)
  //
  // Key discriminators:
  // 1. lemma is つづける (hiragana) - NOT 続ける (kanji)
  // 2. The auxiliary attaches to stem form (連用形)

  r.either(
    // Pattern 1: Standard - stem + つづける as separate tokens
    // Try with inflectionForm constraint first
    (b) => {
      const tsuzukeru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'つづける',
      }, 'tsuzukeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, tsuzukeru, 1);
      b.captureSpan('つづける', stem, tsuzukeru);
    },
    // Pattern 2: Without inflectionForm constraint (GiNZA inconsistency)
    (b) => {
      const tsuzukeru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'つづける',
      }, 'tsuzukeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ', 'NOUN'],
      }, 'stem');
      b.inOrder(stem, tsuzukeru, 1);
      b.captureSpan('つづける', stem, tsuzukeru);
    },
    // Pattern 2b: Special case for suru-verbs (する + つづける)
    // GiNZA may parse "し" as the stem with lemma=する
    (b) => {
      const tsuzukeru = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'つづける',
      }, 'tsuzukeru');
      const stem = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['する'],
      }, 'stem');
      b.inOrder(stem, tsuzukeru, 1);
      b.captureSpan('つづける', stem, tsuzukeru);
    },
    // Pattern 3: Conjugated forms (past, conditional, etc.)
    // Match stem + auxiliary where auxiliary has lemma=つづける
    // but text is conjugated (つづけ, つづけた, etc.)
    (b) => {
      const aux = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemma: 'つづける',
        // Text contains つづけ pattern (may be conjugated)
        textOneOf: [
          'つづけ',      // stem form
          'つづける',    // present
          'つづけて',    // te-form
          'つづけた',    // past
          'つづければ',  // conditional (polite)
          'つづけたら',  // conditional (casual)
        ],
      }, 'aux');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ', 'NOUN'],
      }, 'stem');
      b.inOrder(stem, aux, 1);
      b.captureSpan('つづける', stem, aux);
    },
    // Pattern 4: Compound forms where GiNZA creates single compound token
    (b) => {
      const compound = b.tok({
        posOneOf: ['VERB', 'NOUN'],
        lemmaOneOf: [
          'みまわりつづける',
          'はしりつづける',
          'かきつづける',
          'あるきつづける',
          'なきつづける',
          'みつづける',
          'ふりつづける',
          'つりつづける',
          'なおしつづける',
          'はなし つづける',
          'がんばりつづける',
          'はなしつづける',
          'しつづける',
        ],
      }, 'compound');
      b.capture(compound);
    }
  );
});
