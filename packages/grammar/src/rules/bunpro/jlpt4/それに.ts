import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('それに', (r) => {
  // それに (moreover / besides / in addition)
  // Conjunction adding information to previous clause
  // GiNZA parses as either:
  // - Single token (text=それに)
  // - Two tokens: それ + に
  //
  // Note: This grammar point overlaps with directional use of それに (to that/at that).
  // The rule matches both conjunction and directional uses since GiNZA doesn't
  // consistently distinguish them, and both uses are valid Japanese.

  r.either(
    // Pattern 1: Single token それに
    (b) => {
      const soreni = b.tok({ text: 'それに' }, 'soreni');
      b.capture(soreni);
    },
    // Pattern 2: Two tokens - それ + に
    (b) => {
      const sore = b.tok({ text: 'それ' }, 'sore');
      const ni = b.tok({ text: 'に' }, 'ni');
      b.inOrder(sore, ni, 1);
      b.captureSpan('それに', sore, ni);
    }
  );
});
