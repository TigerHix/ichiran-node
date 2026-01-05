import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('る-verb-neg-past', (r) => {
  // る-verb (ichidan verb) negative-past form
  // Matches: 食べなかった, 見なかった, 寝なかった (casual)
  //          食べませんでした, 見ませんでした, 寝ませんでした (polite)
  //
  // NOTE: We don't constrain by conjugationClass because GiNZA assigns
  // different classes to the same verb depending on reading/context.
  // We rely on lemma dispatch to identify ichidan verbs.
  //
  // GiNZA parsing notes:
  // - GiNZA doesn't always provide inflection forms, so we don't require them
  // - For polite form, "ません" is split into "ませ" + "ん"
  // - "でした" is split into "でし" + "た"
  //
  // Includes both kanji and hiragana variants (e.g., 見る and みる).

  const ichidanVerbs = [
    // 下一段
    '食べる', '寝る', '教える', '始める',
    '出る', '受ける', 'あげる', '閉める', '上げる', '捨てる', '読める', '飲める',
    '開ける', 'つける', '答える',
    'やめる', '消える', '覚える', '別れる', '忘れる',
    // Hiragana variants
    'おしえる', 'わすれる', 'でかける', 'かりる',
    // 上一段
    '見る', 'いる', 'できる', '起きる', '借りる', '降りる',
    '着る', '過ぎる', '落ちる', '似る', '減びる', '感じる',
    // Hiragana variants
    'みる', 'おきる', 'たべる', 'おりる',
  ];

  const branches: Array<(b: typeof r) => void> = [];

  for (const lemma of ichidanVerbs) {
    // Branch for casual negative-past form (～なかった)
    branches.push((b) => {
      const verb = b.verb({
        lemma,
      }, 'verb');
      const nakatta = b.aux({
        lemma: 'ない',
      }, 'nakatta');
      b.auxOf(verb, nakatta);
      b.captureSpan('る-verb-neg-past', verb, nakatta);
    });

    // Branch for polite negative-past form (～ませんでした)
    // Structure: verb (ren'yo) + ませ + ん + でし + た
    // GiNZA parses: 捨てませんでした -> 捨て(verb) + ませ(aux) + ん(aux) + でし(aux) + た(aux)
    // Dependencies: all auxiliaries attach to verb, not sequentially
    branches.push((b) => {
      const verb = b.verb({
        lemma,
      }, 'verb');
      const mase = b.aux({
        text: 'ませ',
        lemma: 'ます',
      }, 'mase');
      const nun = b.tok({
        text: 'ん',
        lemma: 'ぬ',
      }, 'nun');
      const deshi = b.aux({
        text: 'でし',
        lemma: 'です',
      }, 'deshi');
      const ta = b.aux({
        lemma: 'た',
      }, 'ta');

      b.auxOf(verb, mase);
      b.auxOf(verb, nun);
      b.auxOf(verb, deshi);
      b.auxOf(verb, ta);
      b.inOrder(mase, nun, 1);
      b.inOrder(nun, deshi, 1);
      b.inOrder(deshi, ta, 1);
      b.captureSpan('る-verb-neg-past', verb, ta);
    });
  }

  r.either(...branches);
});
