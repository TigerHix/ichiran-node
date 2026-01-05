import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('そばから', (r) => {
  // Pattern: verb (ta-form or dictionary form) + そばから
  // Means "as soon as X happens" - often negative connotation
  // Examples: するそばから, したそばから, 読んだそばから, etc.

  // The verb and そばから can be split by auxiliaries (like て-form)
  // so we allow some distance between them

  // Match verb (any form) followed by そばから
  // We need to capture from the verb to から

  r.either(
    // Pattern 1: Verb + そばから (combined or with auxiliaries)
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const soba = b1.tok({ text: 'そば' }, 'soba');
      const kara = b1.particle('から', 'kara');

      // Allow distance between verb and そばから (for verb chains)
      b1.inOrder(verb, soba, 10);
      b1.inOrder(soba, kara, 1);

      // Capture from verb to から
      b1.captureSpan('そばから', verb, kara);
    },

    // Pattern 2: Handle split tokenization if そば and から are separate
    // This is already covered by Pattern 1 since we use inOrder with distance
  );
});
