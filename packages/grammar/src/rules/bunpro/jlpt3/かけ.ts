import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('かけ', (r) => {
  // かけ is a helper verb meaning "halfway doing" / "incomplete action"
  // Pattern: Verb[masu stem/ren'youkei] + かけ/かける
  // Forms: かけ, かけた, かけて, かけている, かけの, etc.
  //
  // GiNZA parses this as:
  // - Verb stem + かけ: かけ is VERB with lemma=かける
  // - The かけ attaches as compound or aux to the main verb
  //
  // Examples:
  // - わすれかけていた (was half-forgetting)
  // - のみかけのジュース (half-drunk juice)
  // - 読みかけの本 (half-read book)
  // - 死にかけの子犬 (half-dead puppy = about to die)

  // Main pattern: verb with lemma=かける
  const kake = r.tok({ lemma: 'かける' }, 'kake');

  // The verb stem precedes かけ - typically as compound, advcl, or similar
  // We need to match various POS since GiNZA is inconsistent
  const verb = r.tok({
    posOneOf: ['VERB', 'AUX', 'NOUN'],
    lemma: (l) => l !== 'かける' && l !== 'かけ' // exclude kakeru itself
  }, 'verb');

  r.inOrder(verb, kake, 1); // verb immediately before kake

  // Capture the whole construction
  r.captureSpan('かけ', verb, kake);
});
