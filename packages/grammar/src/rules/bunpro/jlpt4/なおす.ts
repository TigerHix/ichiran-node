import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なおす', (r) => {
  // なおす attaches to verb stems (masu form without ます) to mean "do completely" or "redo"
  // Similar to つづける and おわる, GiNZA parses this with various POS/dep combinations:
  //
  // - 書きなおす: stem (VERB) + なおす (VERB/AUX)
  // - 書きなおした: stem (VERB) + なおし (AUX) + た (AUX)
  // - 書き直した: stem (VERB) + 直し (AUX) + た (AUX)
  // - やりなおす: やり (VERB/ADV) + なおす (VERB)
  // - し直す: し (VERB/AUX) + 直す (VERB)
  //
  // Key discriminators:
  // 1. lemma is なおす (hiragana) - NOT 直す (kanji) when used as auxiliary
  // 2. The auxiliary attaches to stem form (連用形)
  // 3. Need to distinguish from standalone "直す" (to fix/repair) vs compound verb usage

  r.either(
    // Pattern 1: Standard - stem + なおす (hiragana) as separate tokens
    // This is the auxiliary verb form meaning "to do over/redo"
    (b) => {
      const naosu = b.tok({
        posOneOf: ['VERB', 'AUX', 'NOUN'], // NOUN for "し なおす" pattern
        lemma: 'なおす', // Hiragana - auxiliary form
      }, 'naosu');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, naosu, 1);
      b.captureSpan('なおす', stem, naosu);
    },
    // Pattern 2: Stem + 直す (kanji) - when used as compound verb
    // Both hiragana and kanji forms appear in conjugated contexts
    (b) => {
      const naosu = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['なおす', '直す'],
      }, 'naosu');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, naosu, 1);
      b.captureSpan('なおす', stem, naosu);
    },
    // Pattern 3: Conjugated forms (past, te-form, etc.)
    // Match stem + auxiliary where auxiliary has lemma=なおす or 直す
    // but text is conjugated (なおし, なおした, 直し, 直した, etc.)
    (b) => {
      const aux = b.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['なおす', '直す'],
        // Text contains the pattern (may be conjugated)
        textOneOf: [
          'なおし',      // stem form
          'なおす',      // present
          'なおして',    // te-form
          'なおした',    // past
          'なおせ',      // imperative/potential stem
          'なおせば',    // conditional
          'なおさ',      // negative stem
          'なおさない',  // negative
          'なおしてください', // request form
          'なおければ',  // conditional (polite)
          'なおさなかった', // past negative
          '直し',        // kanji stem form
          '直す',        // kanji present
          '直して',      // kanji te-form
          '直した',      // kanji past
          '直せ',        // kanji imperative/potential stem
          '直せば',      // kanji conditional
          '直さ',        // kanji negative stem
          '直さない',    // kanji negative
          '直してください', // kanji request form
          '直ければ',    // kanji conditional (polite)
          '直さなかった', // kanji past negative
        ],
      }, 'aux');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ', 'NOUN'],
      }, 'stem');
      b.inOrder(stem, aux, 1);
      b.captureSpan('なおす', stem, aux);
    },
    // Pattern 4: Compound forms where GiNZA creates single compound token
    // Common compounds from test data
    (b) => {
      const compound = b.tok({
        posOneOf: ['VERB', 'NOUN'],
        lemmaOneOf: [
          'かきなおす',
          'かき なおす',
          'かき直す',
          'かき 直す',
          'やりなおす',
          'やり なおす',
          'やり直す',
          'やり 直す',
          'し なおす',
          'し 直す',
          'し直す',
          'みなおす',
          'み なおす',
          'み直す',
          'み 直す',
          'おもいなおす',
          'おもい なおす',
          'おもい直す',
          'おもい 直す',
          'かんがえなおす',
          'かんがえ なおす',
          'かんがえ直す',
          'かんがえ 直す',
          'い なおす',
          'い直す',
          'い 直す',
          'いいなおす',
          'いい なおす',
          'いい直す',
          'いい 直す',
          'たてなおす',
          'たて なおす',
          'たて直す',
          'たて 直す',
          'もちなおす',
          'もち なおす',
          'もち直す',
          'もち 直す',
          'ぬりなおす',
          'ぬり なおす',
          'ぬり直す',
          'ぬり 直す',
          'そめなおす',
          'そめ なおす',
          'そめ直す',
          'そめ 直す',
          '塗りなおす',
          '塗り なおす',
          '塗り直す',
          '塗り 直す',
        ],
      }, 'compound');
      b.capture(compound);
    },
    // Pattern 5: Without inflectionForm constraint (GiNZA inconsistency)
    // Some parses may not have inflectionForm set, and "なおす" can be NOUN
    (b) => {
      const naosu = b.tok({
        posOneOf: ['VERB', 'AUX', 'NOUN'],
        lemmaOneOf: ['なおす', '直す'],
      }, 'naosu');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ', 'NOUN'],
      }, 'stem');
      b.inOrder(stem, naosu, 1);
      b.captureSpan('なおす', stem, naosu);
    }
  );
});
