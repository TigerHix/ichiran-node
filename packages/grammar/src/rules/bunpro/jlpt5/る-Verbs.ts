import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('る-Verbs', (r) => {
  // る-verbs (ichidan verbs): single-conjugation verbs ending in る
  // All conjugation classes starting with 下一段 or 一段 (ichidan)
  //
  // This rule matches ichidan verbs by providing representative example
  // lemmas for each conjugation class. The rule dispatches on these lemmas.
  //
  // Matches both dictionary form (食べる) and polite form (食べます).
  //
  // NOTE: We don't constrain by conjugationClass because GiNZA assigns
  // different classes to the same verb depending on reading/context.
  // For example, 閉める can be 下一段-カ行 (しめる) or 下一段-マ行 (とめる).
  // We rely on lemma dispatch to identify ichidan verbs.

  const lowerMonograde = [
    // カ行
    '受ける', 'あげる', '閉める', 'あける', '出かける',
    // ガ行
    '上げる',
    // サ行
    '出る',
    // タ行
    '捨てる',
    // バ行
    '読める',
    // マ行
    '飲める',
    // ラ行 and A-column
    '食べる', '寝る', '教える', '始める', '開ける', 'つける', '答える',
    '生まれる', '別れる', '疲れる', '忘れる', '覚える',
  ];

  const upperMonograde = [
    // カ行
    '着る',
    // ガ行
    '過ぎる',
    // タ行
    '落ちる',
    // ナ行
    '似る',
    // バ行
    '減びる', '浴びる',
    // ラ行
    '見る', 'いる', 'できる', '起きる', '感じる', '借りる', '降りる',
  ];

  r.either(
    // Dictionary form (終止形-一般 or 連体形-一般)
    // GiNZA sometimes parses verbs as NOUN with tag=動詞-一般
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'NOUN'],
        tagOneOf: ['動詞-一般', '動詞-非自立可能'],
        lemmaOneOf: [...lowerMonograde, ...upperMonograde],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'ruVerb');
      b.capture(verb);
    },
    // Polite form (連用形-一般 + ます)
    (b) => {
      const verb = b.tok({
        pos: 'VERB',
        tagOneOf: ['動詞-一般', '動詞-非自立可能'],
        lemmaOneOf: [...lowerMonograde, ...upperMonograde],
        inflectionForm: '連用形-一般',
      }, 'verb');
      const masu = b.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般',
      }, 'masu');
      b.auxOf(verb, masu);
      b.captureSpan('る-Verbs', verb, masu);
    }
  );
});
