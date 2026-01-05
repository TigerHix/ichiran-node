import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('きらい', (r) => {
  // Match the na-adjective きらい/嫌い (to dislike, hate)
  // Despite ending in い, this is a na-adjective, not an i-adjective
  //
  // GiNZA parses inconsistently:
  //   - パイナップルがきらい → pos=NOUN, lemma=きらい
  //   - 私はスポーツが嫌いです → pos=ADJ, lemma=嫌い
  //   - 大嫌いだ (kanji compound) → pos=ADJ, lemma=大嫌い
  //   - 大きらいだ (mixed writing) → pos=NOUN, lemma=大きらい
  //   - 大きらいな先輩 (mixed writing + な) → pos=ADJ, lemma=大きらい
  //   - 朝ごはんが大きらい (sentence-final) → pos=VERB, lemma=大きらい
  //
  // So we need to match NOUN, ADJ, and VERB, and all lemma variants

  r.either(
    // Branch 1: きらい/嫌い (basic forms, NOUN, ADJ, or VERB)
    (branch) => {
      const kirai = branch.tok({
        lemmaOneOf: ['きらい', '嫌い'],
        posOneOf: ['NOUN', 'ADJ', 'VERB'],
      }, 'kirai');
      branch.capture(kirai);
    },
    // Branch 2: 大嫌い (intensified form, kanji-only, daikirai = "to hate")
    (branch) => {
      const kirai = branch.adj({ lemma: '大嫌い' }, 'kirai');
      branch.capture(kirai);
    },
    // Branch 3: 大きらい (intensified form, mixed writing, daikirai = "to hate")
    (branch) => {
      const kirai = branch.tok({
        lemma: '大きらい',
        posOneOf: ['NOUN', 'ADJ', 'VERB'],
      }, 'kirai');
      branch.capture(kirai);
    }
  );
});
