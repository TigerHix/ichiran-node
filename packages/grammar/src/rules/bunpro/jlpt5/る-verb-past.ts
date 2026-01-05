import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('る-verb-past', (r) => {
  // る-verb (ichidan verb) past form
  // Matches: 食べた, 見た, 寝た (casual)
  //          食べました, 見ました, 寝ました (polite)
  //
  // NOTE: We don't constrain by conjugationClass because GiNZA assigns
  // different classes to the same verb depending on reading/context.
  // We rely on lemma dispatch to identify ichidan verbs.

  const ichidanVerbs = [
    // 下一段 verbs (lower monograde)
    '食べる', '寝る', '教える', '始める',
    '出る', '受ける', 'あげる', '閉める', '上げる', '捨てる', '読める', '飲める',
    '開ける', 'つける', '答える',
    '生まれる', '別れる', '疲れる', '忘れる', '覚える',
    'あける', '出かける',
    'かける',  // to wear (glasses), etc.
    // 上一段 verbs (upper monograde)
    '見る', 'いる', 'できる', '起きる', '借りる', '降りる',
    '着る', '過ぎる', '落ちる', '似る', '減びる', '感じる',
    '浴びる',
  ];

  const branches: Array<(b: typeof r) => void> = [];

  for (const lemma of ichidanVerbs) {
    // Branch for casual past form (～た)
    branches.push((b) => {
      const verb = b.verb({
        lemma,
        inflectionForm: '連用形-一般',
      }, 'verb');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');
      b.auxOf(verb, ta);
      b.captureSpan('る-verb-past', verb, ta);
    });

    // Branch for polite past form (～ました)
    branches.push((b) => {
      const verb = b.verb({
        lemma,
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mashita = b.aux({
        lemma: 'ます',
        inflectionForm: '連用形-一般',
      }, 'mashita');
      b.auxOf(verb, mashita);
      b.captureSpan('る-verb-past', verb, mashita);
    });
  }

  r.either(...branches);
});
