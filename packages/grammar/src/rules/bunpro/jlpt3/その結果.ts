import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('その結果', (r) => {
  // その結果 (sono kekka) - "as a result", "consequently"
  // Conjunction indicating the outcome of a previously mentioned situation
  //
  // Structure: [Previous sentence]. その結果、[Result sentence]
  //
  // Key characteristics:
  // - "その" is a pre-noun adjectival (demonstrative) pointing to the previous context
  // - "結果/けっか" is a noun meaning "result"
  // - Used at the beginning of a sentence
  // - Can be followed by comma (、) or directly by the next clause
  //
  // Test sentences use hiragana "そのけっか", so we match both forms:
  // - その結果 (kanji) - lemma=結果
  // - そのけっか (hiragana) - lemma=けっか
  //
  // GiNZA parses conjunction usage as:
  // - その (DET) with dep=det
  // - 結果/けっか (NOUN) with dep=obl (oblique nominal, not obj/obj)
  //
  // Non-conjunction uses (false positives to avoid):
  // - その結果を報告します (obj - direct object)
  // - その結果を見てみましょう (obj - direct object)

  const sono = r.tok({ text: 'その', dep: 'det' }, 'sono');
  const kekka = r.noun({
    lemmaOneOf: ['結果', 'けっか'],
    depOneOf: ['obl', 'compound'],
  }, 'kekka');

  r.inOrder(sono, kekka, 1);
  r.captureSpan('その結果', sono, kekka);
});
