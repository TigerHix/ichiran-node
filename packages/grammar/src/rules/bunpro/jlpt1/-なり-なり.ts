import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('-なり-なり', (r) => {
  // Pattern: (Verb/Noun) + [に] + なり + (Verb/Noun) + [に] + なり
  // Matches: 売るなり捨てるなり, 両親なり友達なり, 俺になり母さんになり, etc.
  //
  // The に particle is optional (used with directional meaning like "consult WITH")
  // Note: なり can be parsed as SCONJ, PART, AUX, ADP, VERB, or X by GiNZA

  // First element: verb or noun (or pronoun/proper noun/adj)
  const elem1 = r.tok({ posOneOf: ['VERB', 'NOUN', 'PRON', 'PROPN', 'ADJ'] }, 'elem1');

  // Second element: verb or noun (or pronoun/proper noun/adj)
  const elem2 = r.tok({ posOneOf: ['VERB', 'NOUN', 'PRON', 'PROPN', 'ADJ'] }, 'elem2');

  // The two なり particles - accept any POS (text is enough constraint)
  const nari1 = r.tok({ text: 'なり' }, 'nari1');
  const nari2 = r.tok({ text: 'なり' }, 'nari2');

  // Optional に particle before nari1
  r.optional((b) => {
    const ni1 = b.particle('に', 'ni1');
    b.inOrder(elem1, ni1, 1).inOrder(ni1, nari1, 1);
  });

  // If no に, then nari1 immediately follows elem1
  r.inOrder(elem1, nari1, 2);

  // Optional に particle before nari2
  r.optional((b) => {
    const ni2 = b.particle('に', 'ni2');
    b.inOrder(elem2, ni2, 1).inOrder(ni2, nari2, 1);
  });

  // If no に, then nari2 immediately follows elem2
  r.inOrder(elem2, nari2, 2);

  // nari1 must come before elem2
  r.inOrder(nari1, elem2);

  // Capture the full span from first element to second なり
  r.captureSpan('-なり-なり', elem1, nari2);
});
