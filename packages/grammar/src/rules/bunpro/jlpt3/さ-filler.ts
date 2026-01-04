import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('さ-filler', (r) => {
  // さ as a filler particle (like "you know", "like" in English)
  // Used mid-sentence to fill silence while thinking
  // Variations: さ, さぁ, さあ, さー
  // This is different from さ-casualよ (sentence-final) and さ-interjection

  // Pattern 1: さぁ/さあ/さー - must have tag=助詞-終助詞 (not 感動詞-一般)
  // Example: 私さぁ、明日から５連休なんだ (FILLER)
  // Counter: さあ、どうぞ (INTERJECTION - tag=感動詞-一般)
  r.either(
    (b) => {
      const saaLong = b.tok({
        textOneOf: ['さぁ', 'さあ', 'さー'],
        tag: '助詞-終助詞',
      }, 'saaLong');
      b.capture(saaLong);
    },
    // Pattern 2: さ - tagged as PART with dep=mark
    // Note: Ambiguous with さ-casualよ (same GiNZA tags)
    // The distinction is contextual: filler appears mid-sentence, casualよ at sentence end
    // Due to GiNZA limitations, we accept some overlap between these rules
    (b) => {
      const sa = b.tok({
        text: 'さ',
        pos: 'PART',
        tag: '助詞-終助詞',
        dep: 'mark',
      }, 'sa');
      b.capture(sa);
    }
  );
});
