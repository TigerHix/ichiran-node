import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('どれ', (r) => {
  // Interrogative pronoun "dore" - which one (of three or more)
  // Used to ask about which of 3+ things is being chosen
  const dore = r.tok({ lemma: 'どれ', posOneOf: ['PRON', 'NOUN', 'PROPN'] }, 'dore');
  r.capture(dore);
});
