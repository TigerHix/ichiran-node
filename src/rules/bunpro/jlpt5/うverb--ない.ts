import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('うverb--ない', (r) => {
  // u-verb (godan verb) negative form
  // Matches: 歩かない, 歌わない, 泳がない, 話さない, etc. (casual)
  //          歩きません, 歌いません, 泳ぎません, 話しません, etc. (polite)
  //
  // Structure:
  // - VERB with conjugationClass 五段-* (godan verb: 五段-カ行, 五段-ガ行, etc.)
  // - in inflectionForm 未然形-一般 (irrealis form) for casual negative
  // - or in inflectionForm 連用形-一般 (ren'you form) for polite negative (before ません)
  // - followed by AUX ない (lemma=ない, conjugationClass=助動詞-ナイ) for casual
  // - or followed by AUX ます (lemma=ます, inflectionForm=未然形-一般) for polite
  //
  // Negative cases to exclude:
  // - ru-verb negatives: 食べない (下一段), 見ない (上一段)
  // - i-adjective negatives: 高くない (base is ADJ with 連用形)
  // - ある → ない (parsed as ADJ 形容詞, not AUX)
  //
  // Note: ません is parsed by GiNZA as two tokens: "ませ" (ます in 未然形) + "ん" (ぬ)

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
  ] as const;

  // Build branches for all conjugation classes and both forms
  const branches: Array<(b: import('../../../engine/lang.js').LinguisticRuleBuilder) => void> = [];

  for (const conjugationClass of godanClasses) {
    // Branch for casual negative form (～ない)
    branches.push((b) => {
      const verb = b.verb({
        conjugationClass,
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);
      b.captureSpan('うverb--ない', verb, nai);
    });

    // Branch for polite negative form (～ません)
    branches.push((b) => {
      const verb = b.verb({
        conjugationClass,
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mase = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');
      b.auxOf(verb, mase);
      b.captureSpan('うverb--ない', verb, mase);
    });
  }

  r.either(...branches);
});
