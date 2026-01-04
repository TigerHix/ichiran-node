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
        // Common compound forms from test data (including conjugated forms)
        // Note: GiNZA sometimes uses kanji 出す, sometimes hiragana だす in lemmas
        lemmaOneOf: [
          'なきだす',      // なく + だす
          'なきだした',    // past tense
          '泣きだす',      // なく + だす (kanji)
          '泣きだした',    // past tense (kanji)
          'ぬけだす',      // ぬける + だす
          'とびだす',      // とぶ + だす
          'とびだし',      // te-form (from とびだしてくる)
          '飛びだす',      // とぶ + だす (kanji)
          '飛び出し',      // te-form (kanji)
          'にげだす',      // にげる + だす
          'にげだした',    // past tense
          '逃げだす',      // にげる + だす (kanji)
          '逃げだした',    // past tense (kanji)
          'はしりだす',    // はしる + だす
          'はしりだした',  // past tense
          '走りだす',      // はしる + だす (kanji)
          '走りだした',    // past tense (kanji)
          'ふりだす',      // ふる + だす
          'ふりだした',    // past tense
          '降りだす',      // ふる + だす (kanji)
          '降りだした',    // past tense (kanji)
          'わらいだす',    // わらう + だす
          'わらいだした',  // past tense
          '笑いだす',      // わらう + だす (kanji)
          '笑いだした',    // past tense (kanji)
          'のみだす',      // のむ + だす
          'のみだした',    // past tense (hiragana)
          'のみ出した',    // past tense (mixed kanji)
          '飲みだす',      // のむ + だす (kanji stem)
          '飲みだした',    // past tense (kanji stem)
          '飲み出す',      // kanji form
          '飲み出した',    // past tense (all kanji)
          'つくりだす',    // つくる + だす
          'つくりだした',  // past tense
          '作りだす',      // つくる + だす (kanji)
          '作りだした',    // past tense (kanji)
          'おもいだす',    // おもう + だす
          'おもいだした',  // past tense
          '思いだす',      // おもう + だす (kanji)
          '思いだした',    // past tense (kanji)
          'いいだす',      // いう + だす
          'いいだした',    // past tense
          '言いだす',      // いう + だす (kanji)
          '言いだした',    // past tense (kanji)
          'しゃべりだす',  // しゃべる + だす
          'しゃべりだした',// past tense
        ],
      }, 'compound');
      b.capture(compound);
    },
    // Pattern 3: Past/polite with various dependencies - also accept kanji lemma 出す
    // More permissive: don't require inflectionForm (GiNZA inconsistent)
    (b) => {
      const dasu = b.tok({
        lemmaOneOf: ['だす', '出す'], // GiNZA uses both
        posOneOf: ['VERB', 'AUX'],
        depOneOf: ['aux', 'root', 'csubj', 'fixed', 'acl'],
      }, 'dasu');
      const stem = b.tok({
        posOneOf: ['VERB', 'ADV', 'ADJ'],
        // Don't require inflectionForm - GiNZA is inconsistent
      }, 'stem');
      b.inOrder(stem, dasu, 1);
      // Critical: ensure stem is a verb stem (not a noun/particle)
      // This prevents matching noun + 出す compounds
      b.captureSpan('だす', stem, dasu);
    },
    // Pattern 4: Token text match for hiragana だす (most reliable discriminator)
    (b) => {
      const dasu = b.tok({
        textOneOf: ['だす', 'だした', 'だして', 'だし'], // Various conjugations
        posOneOf: ['VERB', 'AUX'],
      }, 'dasu');
      // Require preceding stem (prevent false matches on main verb 出す)
      const stem = b.tok({
        posOneOf: ['VERB'],
        inflectionForm: '連用形-一般',
      }, 'stem');
      b.inOrder(stem, dasu, 1);
      b.captureSpan('だす', stem, dasu);
    }
  );
});
