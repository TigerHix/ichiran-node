import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('も', (r) => {
  // Match the inclusive particle も (mo) - meaning "also", "too", "as well"
  // Examples: 私も先生です (I am also a teacher), 彼も行く (He also goes)
  //
  // This particle replaces other case markers (が, を, に) rather than stacking with them
  // and indicates that whatever is true for one thing is also true for another.
  //
  // Note: も can also mean "even" in contexts like 子どももできる (even a child can do it),
  // but this is a different grammatical usage ( JLPT1 すら / JLPT2 さえ ) which this rule
  // does not attempt to match.
  const mo = r.particle('も', 'mo', { dep: 'case' });
  const noun = r.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM', 'PROPN', 'ADJ'] }, 'noun');
  r.caseMarker(noun, mo);
  r.capture(mo);
});
