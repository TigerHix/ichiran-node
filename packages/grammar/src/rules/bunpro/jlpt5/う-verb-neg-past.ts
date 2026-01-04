import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('う-verb-neg-past', (r) => {
  // Match u-verbs (godan verbs) in negative past form
  // Both casual (～なかった) and polite (～ませんでした, ～なかったです) forms
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
  // POLITE FORM 1 (～ませんでした):
  // GiNZA parses this as:
  // - VERB with conjClass starting with '五段-' (godan/u-verb)
  // - inflectionForm='連用形-一般' (connective form)
  // - AUX ます with inflectionForm='未然形-一般', dep='fixed'
  // - AUX ぬ with dep='fixed'
  // - AUX です with inflectionForm='連用形-一般'
  // - AUX た
  //
  // Examples:
  // - 置きませんでした (didn't place) - from 置く
  // - 遊びませんでした (didn't play) - from 遊ぶ
  // - 死にませんでした (didn't die) - from 死ぬ
  // - ありませんでした (there wasn't) - from ある (special case: 五段-ワア行-アル)
  //
  // POLITE FORM 2 (～なかったです):
  // GiNZA parses this as:
  // - VERB with conjClass starting with '五段-' (godan/u-verb)
  // - inflectionForm='未然形-一般' (irrealis form)
  // - AUX ない with inflectionForm='連用形-促音便'
  // - AUX です with inflectionForm='連用形-一般'
  // - AUX た
  //
  // Examples:
  // - 洗わなかったです (didn't wash - polite)
  // - なかったです (there wasn't - polite)
  //
  // This should NOT match:
  // - ru-verbs (下一段): 食べなかった, 見なかった
  // - i-adjectives (形容詞): 高くなかった

  const godanClasses = [
    '五段-カ行',
    '五段-ガ行',
    '五段-サ行',
    '五段-タ行',
    '五段-ナ行',
    '五段-バ行',
    '五段-マ行',
    '五段-ラ行',
    '五段-ワア行',
    '五段-ワア行-アル', // Special case for ある (existential verb)
  ];

  r.either(
    // Branch 1: Casual negative past form (～なかった)
    (b) => {
      const verb = b.verb({
        conjugationClassOneOf: godanClasses,
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
        conjugationClassOneOf: godanClasses,
        inflectionForm: '連用形-一般', // connective form (stem before ます)
      }, 'verb');

      const masu = b.tok({
        lemma: 'ます',
        pos: 'AUX',
        inflectionForm: '未然形-一般',
      }, 'masu');

      const nu = b.tok({
        lemma: 'ぬ',
        pos: 'AUX',
      }, 'nu');

      const deshi = b.aux({
        lemma: 'です',
        inflectionForm: '連用形-一般',
      }, 'deshi');

      const ta = b.aux({
        lemma: 'た',
        pos: 'AUX',
      }, 'ta');

      b.auxOf(verb, masu);
      b.auxOf(verb, deshi);
      b.auxOf(verb, ta);

      b.inOrder(masu, nu, 1);
      b.inOrder(nu, deshi, 1);
      b.inOrder(deshi, ta, 1);

      // Capture from verb to ta (e.g., 置きませんでした)
      b.captureSpan('う-verb-neg-past', verb, ta);
    },
    // Branch 3: Polite negative past form (～なかったです)
    (b) => {
      const verb = b.verb({
        conjugationClassOneOf: godanClasses,
        inflectionForm: '未然形-一般', // irrealis form (stem before ない)
      }, 'verb');

      const nai = b.aux({
        lemma: 'ない',
        inflectionForm: '連用形-促音便', // nakat- form
      }, 'nai');

      const desu = b.aux({
        lemma: 'です',
        inflectionForm: '連用形-一般',
      }, 'desu');

      const ta = b.aux({
        lemma: 'た',
        pos: 'AUX',
      }, 'ta');

      b.auxOf(verb, nai);
      b.auxOf(verb, desu);
      b.auxOf(verb, ta);

      b.inOrder(nai, desu, 1);
      b.inOrder(desu, ta, 1);

      // Capture from verb to ta (e.g., 洗わなかったです)
      b.captureSpan('う-verb-neg-past', verb, ta);
    }
  );
});
