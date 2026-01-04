import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ていただけませんか', (r) => {
  // This grammar point has TWO forms:
  // 1. ていただけませんか (more humble/polite)
  // 2. てもらえませんか (slightly less formal but still polite)
  //
  // Both mean "Could you please do something" but ていただけませんか is more humble

  // Match both forms by looking for the key distinctive text
  // GiNZA may tokenize "いただけませんか" differently depending on context
  r.either(
    // Pattern 1: ていただけませんか (humble request)
    (b) => {
      // Verb in te-form (て-form)
      const teForm = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');

      // The main verb that connects to the te-form
      const verb = b.verb({}, 'verb');

      // Try matching "いただけ" by text
      const itadaku = b.tok({ text: 'いただけ' }, 'itadaku');

      // Question particle か
      const ka = b.particle('か', 'ka');

      // Structural constraints
      b.headChild(verb, teForm, 'mark');
      b.inOrder(teForm, itadaku, 5);
      b.inOrder(itadaku, ka, 3); // Allow space for "ません"

      b.captureSpan('ていただけませんか', teForm, ka);
    },
    // Pattern 2: てもらえませんか (polite request)
    (b) => {
      // Verb in te-form (て-form)
      const teForm = b.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');

      // The main verb that connects to the te-form
      const verb = b.verb({}, 'verb');

      // Try matching "もらえ" by text
      const morae = b.tok({ text: 'もらえ' }, 'morae');

      // Question particle か
      const ka = b.particle('か', 'ka');

      // Structural constraints
      b.headChild(verb, teForm, 'mark');
      b.inOrder(teForm, morae, 5);
      b.inOrder(morae, ka, 3);

      b.captureSpan('ていただけませんか', teForm, ka);
    }
  );
});
