import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('う-verb-neg-past', (r) => {
  // Match u-verbs (godan verbs) in negative past form
  // Both casual (～なかった) and polite (～ませんでした) forms
  //
  // CASUAL FORM (～なかった):
  // GiNZA parses this as:
  // - VERB with conjClass starting with '五段-' (godan/u-verb)
  // - inflectionForm='未然形-一般' (irrealis form)
  // - AUX ない with lemma=ない, inflectionForm='連用形-促音便'
  //
  // Examples:
  // - 洗わなかった (didn't wash) - from 洗う
  // - 行かなかった (didn't go) - from 行く
  // - 遊ばなかった (didn't play) - from 遊ぶ
  //
  // POLITE FORM (～ませんでした):
  // GiNZA parses this as:
  // - VERB with conjClass starting with '五段-' (godan/u-verb)
  // - inflectionForm='連用形-一般' (connective form)
  // - AUX ます with inflectionForm='未然形-一般'
  // - AUX ぬ
  // - AUX です with inflectionForm='連用形-一般'
  // - AUX た
  //
  // Examples:
  // - 置きませんでした (didn't place) - from 置く
  // - 遊びませんでした (didn't play) - from 遊ぶ
  // - 死にませんでした (didn't die) - from 死ぬ
  //
  // This should NOT match:
  // - ru-verbs (下一段): 食べなかった, 見なかった
  // - i-adjectives (形容詞): 高くなかった

  r.either(
    // Branch 1: Casual negative past form (～なかった)
    (b) => {
      const verb = b.verb({
        conjugationClassOneOf: [
          '五段-カ行',
          '五段-ガ行',
          '五段-サ行',
          '五段-タ行',
          '五段-ナ行',
          '五段-バ行',
          '五段-マ行',
          '五段-ラ行',
          '五段-ワア行',
        ], // all godan verb classes (u-verbs)
        inflectionForm: '未然形-一般', // irrealis form (stem before ない)
      }, 'verb');

      const nakatta = b.aux({
        lemma: 'ない',
        inflectionForm: '連用形-促音便', // nakat- form (connective form)
      }, 'nakatta');

      b.auxOf(verb, nakatta);

      // Capture from verb to nakatta (e.g., 洗わなかった)
      b.captureSpan('う-verb-neg-past', verb, nakatta);
    },
    // Branch 2: Polite negative past form (～ませんでした)
    (b) => {
      const verb = b.verb({
        conjugationClassOneOf: [
          '五段-カ行',
          '五段-ガ行',
          '五段-サ行',
          '五段-タ行',
          '五段-ナ行',
          '五段-バ行',
          '五段-マ行',
          '五段-ラ行',
          '五段-ワア行',
        ], // all godan verb classes (u-verbs)
        inflectionForm: '連用形-一般', // connective form (stem before ます)
      }, 'verb');

      const masenDesita = b.tok({
        lemma: 'ます',
        pos: 'AUX',
        inflectionForm: '未然形-一般',
      }, 'masenDesita');

      // Require the auxiliary to attach to the verb
      b.auxOf(verb, masenDesita);

      // Capture from verb to auxiliary (e.g., 置きませんでした)
      b.captureSpan('う-verb-neg-past', verb, masenDesita);
    }
  );
});
