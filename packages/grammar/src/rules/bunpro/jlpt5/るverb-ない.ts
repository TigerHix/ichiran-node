import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('るverb-ない', (r) => {
  // る-verb (ichidan verb) negative form
  // Matches: 食べない, 見ない, 寝ない (casual)
  //          食べません, 見ません, 寝ません (polite)
  //
  // NOTE: We don't constrain by conjugationClass because GiNZA assigns
  // different classes to the same verb depending on reading/context.
  // We rely on lemma dispatch to identify ichidan verbs.
  //
  // Includes both kanji and hiragana variants (e.g., 見る and みる).

  const ichidanVerbs = [
    // 下一段
    '食べる', '寝る', '教える', '始める',
    '出る', '受ける', 'あげる', '閉める', '上げる', '捨てる', '読める', '飲める',
    '開ける', 'つける', '答える',
    'やめる', '消える', '覚える', '別れる', '忘れる',
    // Hiragana variants
    'おしえる', 'わすれる',
    // 上一段
    '見る', 'いる', 'できる', '起きる', '借りる', '降りる',
    '着る', '過ぎる', '落ちる', '似る', '減びる', '感じる',
    // Hiragana variants
    'みる', 'おりる',
  ];

  const branches: Array<(b: typeof r) => void> = [];

  for (const lemma of ichidanVerbs) {
    // Branch for casual negative form (～ない)
    branches.push((b) => {
      const verb = b.verb({
        lemma,
        inflectionForm: '未然形-一般',
      }, 'verb');
      const nai = b.aux({
        lemma: 'ない',
        conjugationClass: '助動詞-ナイ',
      }, 'nai');
      b.auxOf(verb, nai);
      b.captureSpan('るverb-ない', verb, nai);
    });

    // Branch for polite negative form (～ません)
    branches.push((b) => {
      const verb = b.verb({
        lemma,
        inflectionForm: '連用形-一般',
      }, 'verb');
      const masen = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'masen');
      b.auxOf(verb, masen);
      b.captureSpan('るverb-ない', verb, masen);
    });
  }

  r.either(...branches);
});
