import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いつの間にか', (r) => {
  // いつの間にか - adverbial phrase meaning "before one knew it, unconsciously, without noticing"
  // Fixed expression: いつ + の + 間 + に + か
  // Indicates something happened without the speaker noticing or realizing
  //
  // Examples:
  // - いつの間にか夜になっていた。
  // - いつの間にか春が来ていた。
  // - 気づいたら、いつの間にか冬になっていた。

  // Match the fixed phrase: いつの間にか (or いつのまにか)
  const itsu = r.tok({ text: 'いつ' }, 'itsu');
  const no = r.particle('の', 'no');
  // Accept both 間 (kanji) and ま (hiragana) - they're the same word
  const ma = r.tok({ lemmaOneOf: ['間', 'ま'], pos: 'NOUN' }, 'ma');
  const ni = r.particle('に', 'ni');
  const ka = r.particle('か', 'ka');

  // Require tokens in sequence with small gaps (fixed phrase)
  r.inOrder(itsu, no, 1);
  r.inOrder(no, ma, 1);
  r.inOrder(ma, ni, 1);
  r.inOrder(ni, ka, 1);

  // Capture the entire span from いつ to か
  r.captureSpan('いつの間にか', itsu, ka);
});
