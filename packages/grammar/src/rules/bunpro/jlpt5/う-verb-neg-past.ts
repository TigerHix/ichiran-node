import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('う-verb-neg-past', (r) => {
  // Match u-verbs (godan verbs) in negative past form (～なかった)
  // Pattern: verb stem + なかった
  //
  // GiNZA parses this as:
  // - VERB with conjClass starting with '五段-' (godan/u-verb)
  // - inflectionForm='未然形-一般' (irrealis form)
  // - AUX ない with lemma=ない, inflectionForm='連用形-促音便'
  // - AUX た with lemma=た
  //
  // Examples:
  // - 洗わなかった (didn't wash) - from 洗う
  // - 行かなかった (didn't go) - from 行く
  // - 遊ばなかった (didn't play) - from 遊ぶ
  //
  // This should NOT match:
  // - ru-verbs (下一段): 食べなかった, 見なかった
  // - i-adjectives (形容詞): 高くなかった

  const verb = r.verb({
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

  const nakatta = r.aux({
    lemma: 'ない',
    inflectionForm: '連用形-促音便', // nakat- form (connective form)
  }, 'nakatta');

  r.auxOf(verb, nakatta);

  // Capture from verb to nakatta (e.g., 洗わなかった)
  r.captureSpan('う-verb-neg-past', verb, nakatta);
});
