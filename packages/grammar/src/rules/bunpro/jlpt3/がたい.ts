import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('がたい', (r) => {
  // がたい (gatai) - auxiliary suffix meaning "hard to" or "difficult to"
  // Attaches to verb stem (masu form) to indicate something is extremely difficult
  // or nearly impossible to do. Stronger than にくい, focuses on the inherent
  // difficulty of the task itself rather than the speaker's feeling.
  //
  // Examples:
  //   信じがたい (hard to believe)
  //   捨てがたい (hard to give up)
  //   耐えがたい (hard to bear)
  //   言いがたい (hard to say)
  //
  // GiNZA parses this pattern in multiple ways:
  // 1. Verb stem (連用形) + がたい as separate tokens
  // 2. Verb stem + がたい parsed as single ADJ token
  // 3. Various dependency relations (advcl, compound, fixed)

  r.either(
    // Branch 1: がたい as auxiliary with dep=aux attached to verb
    // Most common pattern for verb stem + aux
    (b) => {
      const gatai = b.aux({
        lemma: 'がたい',
        dep: 'aux',
      }, 'gatai');
      b.capture(gatai);
    },

    // Branch 2: がたい as auxiliary with dep=fixed
    // Alternative parsing for some verb forms
    (b) => {
      const gatai = b.aux({
        lemma: 'がたい',
        dep: 'fixed',
      }, 'gatai');
      b.capture(gatai);
    },

    // Branch 3: Verb stem (ren'youkei) + がたい as advcl modifier
    // Stem is syntactic head, がたい modifies it
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const gatai = b.aux({
        lemma: 'がたい',
      }, 'gatai');
      b.headChild(stem, gatai, 'advcl');
      b.captureSpan('がたい', stem, gatai);
    },

    // Branch 4: Verb stem + がたい with compound dependency
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const gatai = b.aux({
        lemma: 'がたい',
      }, 'gatai');
      b.headChild(stem, gatai, 'compound');
      b.captureSpan('がたい', stem, gatai);
    },

    // Branch 5: がたい as single ADJ token
    // GiNZA sometimes parses "verb+がたい" as one adjective token
    (b) => {
      const gatai = b.adj({
        lemma: 'がたい',
      }, 'gatai');
      b.capture(gatai);
    },

    // Branch 6: Any token with lemma=がたい (catch-all for unexpected parsings)
    (b) => {
      const gatai = b.tok({
        lemma: 'がたい',
      }, 'gatai');
      b.capture(gatai);
    }
  );
});
