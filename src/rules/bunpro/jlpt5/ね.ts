import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ね', (r) => {
  // Match sentence-ending particle ね (for agreement/confirmation)
  // Note: This matches any ね particle since GiNZA doesn't reliably distinguish
  // sentence-final vs filler usage based on dependency structure alone.
  const ne = r.tok({ text: 'ね', pos: 'PART' }, 'ne');
  r.capture(ne);
});
