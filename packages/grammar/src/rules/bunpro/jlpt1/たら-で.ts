import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: たら-で (tara-de pattern)
 *
 * Matches the pattern: Conditional[X] + X + で (repetition with conditional + conjunction)
 *
 * Examples:
 * - Verb with tara: 失敗したら失敗したで (tara-form + repeated verb + ta + de)
 * - Verb with ba: 降れば降ったで (ba-form + repeated verb + ta + de)
 * - Verb with tara (merged): したらしたで (merged tara + merged shita + de)
 * - I-adj with ba: 低ければひくいで (ba-form + repeated adj + de, no ta)
 * - Na-adj with nara: 簡単なら簡単で (nara-form + repeated adj + de, no ta)
 *
 * This is a conversational expression indicating "in the case of X" or "once X happens",
 * often used to acknowledge both positive and negative aspects of a situation.
 *
 * The pattern captures from the first conditional marker to the final で particle.
 */
export default bunproLinguisticRule('たら-で', (r) => {
  // All conditionals we want to match
  const conditionals = r.tok({
    textOneOf: ['たら', 'ったら', 'だら', 'なら', 'ば', 'れば'],
  }, 'conditional');

  // All past tense forms we want to match
  const taForms = r.tok({
    textOneOf: ['た', 'した', 'いた', 'った', 'だった'],
  }, 'ta');

  // Conjunction で
  const de = r.tok({
    text: 'で',
  }, 'de');

  // Pattern 1: Conditional immediately followed by ta-form, then immediately by de
  // Matches: 失敗したら失敗したで, したらしたで
  r.inOrder(conditionals, taForms);
  r.inOrder(taForms, de, 1);
  r.captureSpan('たら-で', conditionals, de);
});
