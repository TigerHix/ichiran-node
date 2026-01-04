import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('の-noun-ommission', (r) => {
  // の as noun omission nominalizer
  // Pattern: [NOUN/PROPN/PRON/Clause] の (end of sentence or before copula/particle)
  // The noun after の is omitted because it was previously mentioned or is clear from context
  //
  // This is DISTINCT from the basic の (possessive) rule:
  // - の-noun-ommission: [Owner/Clause] + の [END/copula/particle] (noun omitted)
  //   Examples: たけしさんのです, 私の, 誰の？, 友達ので行った, あなたが乗っているの
  // - の (possessive): Owner + の + NOUN (explicit second noun)
  //   Examples: 私の本, たけしさんの車

  r.either(
    // Pattern 1: の + copula/aux (です, だ, じゃない, etc.)
    // Key: copula must directly modify the phrase ending in の (no intervening noun)
    // e.g., たけしさんのです, 私のじゃない, ヨシのだ, あなたが乗っているのです
    (b1) => {
      const no = b1.particle('の', 'no');
      const copula = b1.tok({ posOneOf: ['AUX', 'VERB'] }, 'copula');

      // Require copula is immediately after or one token after の
      // This ensures there's no noun between の and copula
      b1.inOrder(no, copula, 2);
      b1.capture(no);
    },
    // Pattern 2: の + particle (で, etc.) - particle must IMMEDIATELY follow の
    // e.g., 友達ので行ったよ (I went on [a bicycle] of a friend)
    (b2) => {
      const no = b2.particle('の', 'no');
      const particle = b2.tok({ pos: 'ADP', dep: 'case' }, 'particle');

      b2.inOrder(no, particle, 1);  // Must immediately follow
      b2.capture(no);
    },
    // Pattern 3: の at sentence end (with punctuation)
    // e.g., 誰の？, あなたの？
    (b3) => {
      const no = b3.particle('の', 'no');
      const punct = b3.tok({ pos: 'PUNCT' }, 'punct');

      b3.inOrder(no, punct, 2);
      b3.capture(no);
    }
  );
});
