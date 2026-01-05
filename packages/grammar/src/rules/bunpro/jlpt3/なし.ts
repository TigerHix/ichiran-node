import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なし', (r) => {
  // Linguistic view:
  // なし is the literary form (文語形) of ない used to indicate "without X" (exclusion)
  // It's the opposite of あり (with X)
  //
  // Key discriminators from regular ない:
  // - lemma=なし (when GiNZA parses it as NOUN)
  // - lemma=ない with inflectionForm=連用形-一般 (nominal form)
  // - Followed by copula だ/です, particle の/で/では, or standalone
  //
  // Structural patterns:
  // 1. Noun modifier: [noun] なし の [noun] → "noun WITHOUT noun"
  //    肉なしの人気料理
  // 2. Instrumental: [noun] なし で [verb] → "do [verb] WITHOUT [noun]"
  //    許可なしで公園に店を出さないで下さい
  // 3. Conditional: [noun] なし では [verb/neg] → "cannot [verb] WITHOUT [noun]"
  //    スマホなしでは生活できない
  // 4. Predicative: [noun] なし だ → "is without [noun]"
  //    今月はもうお金なしだ
  // 5. Standalone: [noun] なし (at end of clause or sentence)
  //    先輩なしにしよう

  r.either(
    // Pattern 1: なし + の (noun modifier)
    // [noun] なし の [noun] = "noun WITHOUT noun"
    // GiNZA parses as: なし with lemma=なし, posOneOf=['NOUN', 'VERB']
    (b) => {
      const nashi = b.tok({
        lemma: 'なし',
        posOneOf: ['NOUN', 'VERB'],
      }, 'nashi');
      const no = b.particle('の', 'no');
      b.inOrder(nashi, no, 1);
      b.captureSpan('なし', nashi, nashi);
    },
    // Pattern 2: なし + で (instrumental case)
    // [noun] なし で [verb] = "do [verb] WITHOUT [noun]"
    // 許可なしで (without permission)
    // パスワードなしで (without password)
    (b) => {
      const nashi = b.tok({
        lemma: 'なし',
        posOneOf: ['NOUN', 'VERB'],
      }, 'nashi');
      const de = b.particle('で', 'de');
      b.inOrder(nashi, de, 1);
      b.captureSpan('なし', nashi, nashi);
    },
    // Pattern 3: なし + では (conditional/emphatic particle)
    // [noun] なし では [verb] = "cannot [verb] WITHOUT [noun]"
    // コンテクストなしでは (without context)
    // スマホなしでは (without smartphone)
    (b) => {
      const nashi = b.tok({
        lemma: 'なし',
        posOneOf: ['NOUN', 'VERB'],
      }, 'nashi');
      const dewa = b.tok({ text: 'では' }, 'dewa');
      b.inOrder(nashi, dewa, 1);
      b.captureSpan('なし', nashi, nashi);
    },
    // Pattern 4: なし + だ (copula)
    // [noun] なし だ = "is without [noun]"
    // 今月はもうお金なしだ (I'm already without money this month)
    // その考えは間違いなしだ (That way of thinking is without error)
    (b) => {
      const nashi = b.tok({
        lemma: 'なし',
        posOneOf: ['NOUN', 'VERB'],
      }, 'nashi');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.auxOf(nashi, da);
      b.captureSpan('なし', nashi, da);
    },
    // Pattern 5: なし + です (polite copula)
    // [noun] なし です = "is without [noun]" (polite)
    (b) => {
      const nashi = b.tok({
        lemma: 'なし',
        posOneOf: ['NOUN', 'VERB'],
      }, 'nashi');
      const desu = b.aux({ lemma: 'だ' }, 'desu');
      b.auxOf(nashi, desu);
      b.captureSpan('なし', nashi, desu);
    },
    // Pattern 6: ない + 連用形-一般 + だ (alternative parse)
    // Some sentences may parse as ない (lemma) with 連用形-一般 form
    // 間違いなしだ → may parse as ない with inflectionForm=連用形-一般
    (b) => {
      const nashi = b.verb({
        lemma: 'ない',
        inflectionForm: '連用形-一般',
      }, 'nashi');
      const da = b.aux({ lemma: 'だ' }, 'da');
      b.auxOf(nashi, da);
      b.captureSpan('なし', nashi, da);
    },
    // Pattern 7: なし (standalone at end of clause or sentence)
    // Must be followed by copula, particle, or end of sentence
    // 昼飯なしにする (I'll skip lunch)
    // 先輩なしにしよう (Let's do it without senpai)
    (b) => {
      const nashi = b.tok({
        lemma: 'なし',
        posOneOf: ['NOUN', 'VERB'],
      }, 'nashi');
      b.captureSpan('なし', nashi, nashi);
    }
  );
});
