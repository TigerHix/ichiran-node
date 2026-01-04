import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('たい', (r) => {
  // たい - auxiliary verb meaning "want to"
  // Attaches to verb stems (masu-stem) to express desire
  // Functions like an i-adjective (conjugates as たくない, たかった, etc.)
  //
  // Forms to match:
  // - Present: 行きたい, 食べたい
  // - Negative: 行きたくない, 食べたくない
  // - Past: 行きたかった, 食べたかった
  // - Past negative: 行きたくなかった, 食べたくなかった
  // - Polite forms with です: 行きたいです, 行きたくないです, etc.
  //
  // The たい auxiliary attaches to:
  // - Godan verbs in 連用形-一般 (ren'youkei / masu stem): 行き + たい
  // - Ichidan verbs in 連用形-一般: 食べ + たい
  // - Irregular verbs: し + たい, し + たかった, 来(き) + たい
  //
  // GiNZA parses たい and its conjugations as:
  // - たい: AUX with lemma=たい
  // - たくない: たく (AUX) + ない (AUX/ADJ)
  // - たかった: たかっ (AUX) + た (AUX)
  // - たくなかった: たく (AUX) + ない (AUX/ADJ) + た (AUX)

  r.either(
    // Pattern 1: Simple affirmative (～たい)
    // 行きたい, 食べたい, したい, 来(き)たい
    (b) => {
      const tai = b.aux({ lemma: 'たい' }, 'tai');
      b.captureSpan('たい', tai, tai);
    },

    // Pattern 2: Simple negative (～たくない)
    // 行きたくない, 食べたくない
    (b) => {
      const taku = b.aux({ text: 'たく', lemma: 'たい', inflectionForm: '連用形-一般' }, 'taku');
      const nai = b.aux({ lemma: 'ない', conjugationClass: '助動詞-ナイ' }, 'nai');
      b.auxOf(taku, nai);
      b.captureSpan('たい', taku, nai);
    },

    // Pattern 3: Past affirmative (～たかった)
    // 行きたかった, 食べたかった
    (b) => {
      const taku = b.aux({ text: 'たかっ', lemma: 'たい', inflectionForm: '連用形-促音便' }, 'taku');
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(taku, ta);
      b.captureSpan('たい', taku, ta);
    },

    // Pattern 4: Past negative (～たくなかった)
    // 行きたくなかった, 食べたくなかった
    (b) => {
      const taku = b.aux({ text: 'たく', lemma: 'たい', inflectionForm: '連用形-一般' }, 'taku');
      const nai = b.aux({ lemma: 'ない', conjugationClass: '助動詞-ナイ' }, 'nai');
      const ta = b.aux({ lemma: 'た' }, 'ta');
      b.auxOf(taku, nai);
      b.auxOf(nai, ta);
      b.captureSpan('たい', taku, ta);
    },

    // Pattern 5: Polite affirmative (～たいです)
    // 行きたいです, 食べたいです
    (b) => {
      const tai = b.aux({ lemma: 'たい' }, 'tai');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(tai, desu);
      b.captureSpan('たい', tai, desu);
    },

    // Pattern 6: Polite negative (～たくないです)
    // 行きたくないです, 食べたくないです
    (b) => {
      const taku = b.aux({ text: 'たく', lemma: 'たい', inflectionForm: '連用形-一般' }, 'taku');
      const nai = b.aux({ lemma: 'ない', conjugationClass: '助動詞-ナイ' }, 'nai');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(taku, nai);
      b.auxOf(nai, desu);
      b.captureSpan('たい', taku, desu);
    },

    // Pattern 7: Polite negative with ありません (～たくありません)
    // 行きたくありません, 食べたくありません
    (b) => {
      const taku = b.aux({ text: 'たく', lemma: 'たい', inflectionForm: '連用形-一般' }, 'taku');
      const arimasen = b.aux({ lemma: 'あります', inflectionForm: '未然形-一般' }, 'arimasen');
      b.auxOf(taku, arimasen);
      b.captureSpan('たい', taku, arimasen);
    },

    // Pattern 8: Polite past affirmative (～たかったです)
    // 行きたかったです, 食べたかったです
    (b) => {
      const taku = b.aux({ text: 'たかっ', lemma: 'たい', inflectionForm: '連用形-促音便' }, 'taku');
      const ta = b.aux({ lemma: 'た' }, 'ta');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(taku, ta);
      b.auxOf(ta, desu);
      b.captureSpan('たい', taku, desu);
    },

    // Pattern 9: Polite past negative (～たくなかったです)
    // 行きたくなかったです, 食べたくなかったです
    (b) => {
      const taku = b.aux({ text: 'たく', lemma: 'たい', inflectionForm: '連用形-一般' }, 'taku');
      const nai = b.aux({ lemma: 'ない', conjugationClass: '助動詞-ナイ' }, 'nai');
      const ta = b.aux({ lemma: 'た' }, 'ta');
      const desu = b.aux({ lemma: 'です' }, 'desu');
      b.auxOf(taku, nai);
      b.auxOf(nai, ta);
      b.auxOf(ta, desu);
      b.captureSpan('たい', taku, desu);
    },

    // Pattern 10: Polite past negative with ありませんでした (～たくありませんでした)
    // 行きたくありませんでした, 食べたくありませんでした
    (b) => {
      const taku = b.aux({ text: 'たく', lemma: 'たい', inflectionForm: '連用形-一般' }, 'taku');
      const arimasen = b.aux({ lemma: 'あります', inflectionForm: '未然形-一般' }, 'arimasen');
      const deshita = b.aux({ lemma: 'です', inflectionForm: '連用形-一般' }, 'deshita');
      b.auxOf(taku, arimasen);
      b.auxOf(arimasen, deshita);
      b.captureSpan('たい', taku, deshita);
    }
  );
});
