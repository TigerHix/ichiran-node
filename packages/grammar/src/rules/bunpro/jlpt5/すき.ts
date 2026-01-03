import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('すき', (r) => {
  // Match the na-adjective すき/好き (to like, be fond of)
  // Despite ending in い, this is a na-adjective, not an i-adjective
  //
  // GiNZA parses inconsistently:
  //   - パイナップルがすき → pos=NOUN, lemma=すき
  //   - 私はスポーツが好きです → pos=ADJ, lemma=好き
  //   - 大好きだ (kanji compound) → pos=ADJ, lemma=大好き
  //   - 大すきだ (mixed writing) → pos=NOUN, lemma=大すき
  //
  // So we need to match NOUN, ADJ, and VERB, and all lemma variants
  //
  // This is the antonym of きらい (to dislike/hate)

  r.either(
    // Branch 1: すき/好き (basic forms, NOUN, ADJ, or VERB)
    (branch) => {
      const suki = branch.tok({
        lemmaOneOf: ['すき', '好き'],
        posOneOf: ['NOUN', 'ADJ', 'VERB'],
      }, 'suki');
      branch.capture(suki);
    },
    // Branch 2: 大好き (intensified form, kanji-only, daisuki = "to love")
    (branch) => {
      const suki = branch.adj({ lemma: '大好き' }, 'suki');
      branch.capture(suki);
    },
    // Branch 3: 大すき (intensified form, hiragana-only, daisuki = "to love")
    (branch) => {
      const suki = branch.tok({
        lemma: '大すき',
        posOneOf: ['NOUN', 'ADJ', 'VERB'],
      }, 'suki');
      branch.capture(suki);
    }
  );
});
