import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('さ-casualよ', (r) => {
  // さ is a casual sentence-ending particle (similar to よ but softer/masculine)
  // Used at the end of casual speech for emphasis, to convey new information,
  // or as a filler/emphasis particle
  // Pattern: word + さ (sentence-final particle)
  // Must be followed by punctuation (end of sentence)

  // さ particle as sentence ender - must be followed by punctuation
  // Variations: さ, さぁ, さあ, さー (all map to lemma=さ in GiNZA)
  const sa = r.tok({ text: 'さ', pos: 'PART', dep: 'mark' }, 'sa');
  const punct = r.tok({ pos: 'PUNCT' });
  r.inOrder(sa, punct, 1);
  r.capture(sa);
});
