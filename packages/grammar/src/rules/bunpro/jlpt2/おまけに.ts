import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('おまけに', (r) => {
  // おまけに (omakeni) - "on top of that, in addition, to make matters worse"
  //
  // A conjunction/adverb used to add something to a previous statement,
  // often with an emphatic or negative nuance. Similar to "what's more"
  // or "to make matters worse".
  //
  // Can be written in hiragana (おまけに) or with kanji (御負けに)
  // GiNZA may parse as single token or split into おまけ (NOUN) + に (PART)

  r.either(
    // Pattern 1: Single token おまけに (ADV, CCONJ, or PART)
    (b) => {
      const omakeni = b.tok({
        textOneOf: ['おまけに', '御負けに'],
      }, 'omakeni');
      b.capture(omakeni);
    },

    // Pattern 2: Split into おまけ (NOUN) + に (particle/aux)
    (b) => {
      const omake = b.tok({
        textOneOf: ['おまけ', '御負け'],
        pos: 'NOUN',
      }, 'omake');
      const ni = b.particle('に', 'ni');
      b.inOrder(omake, ni, 1);
      b.captureSpan('おまけに', omake, ni);
    }
  );
});
