import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('いよいよ', (r) => {
  // いよいよ - adverb meaning "at last, finally" or "more and more, increasingly"
  // A very casual way of saying finally. Usually indicating great effort along the way,
  // or having reached the most important stage.
  //
  // Patterns:
  // - いよいよ + phrase (sentence/paragraph initial): いよいよ明日は試験だ
  // - いよいよ + verb: いよいよ風が強くなってきた
  //
  // Also written as 愈々
  //
  // Examples:
  // - いよいよ明日は試験だ。
  // - いよいよ俺の番だ。
  // - 台風が接近するにつれて、いよいよ風が強くなってきた。

  // Accept both hiragana and kanji forms
  const iyoiyo = r.tok({
    textOneOf: ['いよいよ', '愈々'],
  }, 'iyoiyo');

  // For いよいよ, we need to capture the phrase it modifies
  // It's a sentence/paragraph adverb that modifies what comes after it
  r.either(
    // Pattern 1: いよいよ + verb (most common pattern)
    // いよいよ明日は試験だ, いよいよ俺の番だ, いよいよ風が強くなってきた
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(iyoiyo, verb, 5);
      b.captureSpan('いよいよ', iyoiyo, verb);
    },

    // Pattern 2: いよいよ + sentence (catch-all)
    // Covers sentences where いよいyo modifies the entire sentence
    (b) => {
      // Just capture いよいよ itself when followed by any content
      b.capture(iyoiyo);
    }
  );
});
