import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('にくい', (r) => {
  // にくい (nikui) - auxiliary suffix meaning "hard to" or "difficult to"
  // Attaches to verb stem (masu form) to indicate something is difficult to do
  // due to the speaker's lack of skill or personal feelings. Less formal than がたい.
  //
  // Examples:
  //   食べにくい (hard to eat)
  //   言いにくい (hard to say)
  //   覚えにくい (hard to memorize)
  //   しにくい (hard to do)
  //
  // GiNZA parses this pattern in multiple ways:
  // 1. Verb stem (連用形) + にくい as separate tokens
  // 2. Verb stem + にくい parsed as single ADJ token
  // 3. Various dependency relations (aux, fixed, compound, advcl)

  r.either(
    // Branch 1: にくい as auxiliary with dep=aux attached to verb
    // Most common pattern for verb stem + aux
    (b) => {
      const nikui = b.aux({
        lemma: 'にくい',
        dep: 'aux',
      }, 'nikui');
      b.capture(nikui);
    },

    // Branch 2: にくい as auxiliary with dep=fixed
    // Alternative parsing for some verb forms
    (b) => {
      const nikui = b.aux({
        lemma: 'にくい',
        dep: 'fixed',
      }, 'nikui');
      b.capture(nikui);
    },

    // Branch 3: Verb stem (ren'youkei) + にくい with advcl dependency
    // Stem is syntactic head, にくい modifies it
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const nikui = b.aux({
        lemma: 'にくい',
      }, 'nikui');
      b.headChild(stem, nikui, 'advcl');
      b.captureSpan('にくい', stem, nikui);
    },

    // Branch 4: Verb stem + にくい with compound dependency
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const nikui = b.aux({
        lemma: 'にくい',
      }, 'nikui');
      b.headChild(stem, nikui, 'compound');
      b.captureSpan('にくい', stem, nikui);
    },

    // Branch 5: にくい as single ADJ token
    // GiNZA sometimes parses "verb+にくい" as one adjective token
    (b) => {
      const nikui = b.adj({
        lemma: 'にくい',
      }, 'nikui');
      b.capture(nikui);
    },

    // Branch 6: Any token with lemma=にくい (catch-all for unexpected parsings)
    (b) => {
      const nikui = b.tok({
        lemma: 'にくい',
      }, 'nikui');
      b.capture(nikui);
    }
  );
});
