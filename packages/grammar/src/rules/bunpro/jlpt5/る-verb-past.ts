import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('る-verb-past', (r) => {
  // る-verb (ichidan verb) past form
  // Matches: 食べた, 見た, 寝た (casual)
  //          食べました, 見ました, 寝ました (polite)

  const ichidanVerbs = [
    // 下一段
    { lemma: '食べる', class: '下一段-ラ行' },
    { lemma: '寝る', class: '下一段-ラ行' },
    { lemma: '教える', class: '下一段-ラ行' },
    { lemma: '始める', class: '下一段-ラ行' },
    { lemma: '見る', class: '上一段-ラ行' },
    { lemma: 'いる', class: '上一段-ラ行' },
    { lemma: 'できる', class: '上一段-ラ行' },
    { lemma: '起きる', class: '上一段-ラ行' },
    { lemma: '借りる', class: '上一段-ラ行' },
    { lemma: '降りる', class: '上一段-ラ行' },
    { lemma: '着る', class: '上一段-カ行' },
    { lemma: '出る', class: '下一段-サ行' },
    { lemma: '受ける', class: '下一段-カ行' },
    { lemma: 'あげる', class: '下一段-カ行' },
    { lemma: '閉める', class: '下一段-カ行' },
    { lemma: '上げる', class: '下一段-ガ行' },
    { lemma: '捨てる', class: '下一段-タ行' },
    { lemma: '読める', class: '下一段-バ行' },
    { lemma: '飲める', class: '下一段-マ行' },
    { lemma: '過ぎる', class: '上一段-ガ行' },
    { lemma: '落ちる', class: '上一段-タ行' },
    { lemma: '似る', class: '上一段-ナ行' },
    { lemma: '減びる', class: '上一段-バ行' },
    { lemma: '感じる', class: '上一段-ラ行' },
    { lemma: '開ける', class: '下一段-ラ行' },
    { lemma: 'つける', class: '下一段-ラ行' },
    { lemma: '答える', class: '下一段-ラ行' },
  ];

  const branches: Array<(b: typeof r) => void> = [];

  for (const { lemma, class: conjugationClass } of ichidanVerbs) {
    // Branch for casual past form (～た)
    branches.push((b) => {
      const verb = b.verb({
        lemma,
        conjugationClass,
        inflectionForm: '連用形-一般',
      }, 'verb');
      const ta = b.aux({
        lemma: 'た',
        pos: 'AUX',
      }, 'ta');
      b.auxOf(verb, ta);
      b.captureSpan('る-verb-past', verb, ta);
    });

    // Branch for polite past form (～ました)
    branches.push((b) => {
      const verb = b.verb({
        lemma,
        conjugationClass,
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
