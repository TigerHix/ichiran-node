import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('つい', (r) => {
  // つい - adverb meaning "unintentionally, carelessly, against one's better judgment"
  // Used to express doing something without thinking or despite knowing better
  // Usually paired with てしまう to emphasize the unintentional nature
  //
  // Patterns:
  // - つい + verb: つい食べてしまう, つい笑ってしまう, つい見ちゃう
  // - つい + verb-te-shimau: ついイライラしてしまう, ついやってしまう
  // - つい + noun: つい楽な方, ついさっきまで (less common)
  //
  // Key discriminators:
  // - Different from ついに (finally) - ついに is followed by verb directly
  // - Different from つい as "directly/soon" (spatial/temporal proximity)
  // - Usually followed by verb phrase indicating unintentional action
  //
  // Note: GiNZA typically tags つい as ADV in this context

  const tsui = r.adv({
    text: 'つい',
  }, 'tsui');

  r.either(
    // Pattern 1: つい + verb (most common pattern)
    // つい食べてしまう, つい笑ってしまう, つい見ちゃう
    // ついイライラしてしまう, ついやってしまう
    (b) => {
      const verb = b.verb({}, 'verb');
      b.inOrder(tsui, verb, 10);
      b.captureSpan('つい', tsui, verb);
    },

    // Pattern 2: つい + noun (direct modification, less common)
    // つい楽な方, ついさっきまで
    // Only match if followed by specific common nouns
    (b) => {
      const noun = b.noun({}, 'noun');
      b.inOrder(tsui, noun, 3);
      b.captureSpan('つい', tsui, noun);
    }
  );
});
