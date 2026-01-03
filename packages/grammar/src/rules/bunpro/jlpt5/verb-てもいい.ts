import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: verb-てもいい (Verb + てもいい - May do.../It's okay to do...)
 *
 * Matches verb て-form + も + いい, meaning "may do", "it's okay to do", "can do" (permission)
 * The も adds emphasis (even if) and いい means "good/okay"
 *
 * Examples:
 * - 行ってもいい (may go)
 * - 食べてもいい (may eat)
 * - してもいい (may do)
 * - 飲んでもいい (may drink)
 *
 * Handles both て and で forms (e.g., 行って vs 飲んで)
 *
 * This rule should NOT match:
 * - Simple て form without もいい (e.g., 行って - just te-form)
 * - て-form used for other purposes (e.g., 行ってください - please go)
 */
export default linguisticRule('verb-てもいい', (r) => {
  r.either(
    // Branch 1: Verb + て + も + いい (e.g., 行ってもいい)
    (b1) => {
      const verb = b1.verb({}, 'verb');

      const te = b1.tok(
        {
          text: 'て',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'te'
      );

      const mo = b1.tok(
        {
          text: 'も',
          pos: 'ADP',
          dep: 'fixed',
        },
        'mo'
      );

      const ii = b1.tok(
        {
          lemmaOneOf: ['いい', 'よい'],
          pos: 'AUX',
          dep: 'fixed',
          conjugationClass: '形容詞',
        },
        'ii'
      );

      // Require structural relationships
      // te --mark--> verb
      b1.headChild(verb, te, 'mark');
      // mo --fixed--> te
      b1.headChild(te, mo, 'fixed');
      // ii --fixed--> te
      b1.headChild(te, ii, 'fixed');

      b1.inOrder(verb, te);
      b1.inOrder(te, mo, 1);
      b1.inOrder(mo, ii, 1);

      // Capture the full pattern
      b1.captureSpan('verb-てもいい', verb, ii);
    },
    // Branch 2: Verb + で + も + いい (e.g., 飲んでもいい)
    (b2) => {
      const verb = b2.verb({}, 'verb');

      const de = b2.tok(
        {
          text: 'で',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'de'
      );

      const mo = b2.tok(
        {
          text: 'も',
          pos: 'ADP',
          dep: 'fixed',
        },
        'mo'
      );

      const ii = b2.tok(
        {
          lemmaOneOf: ['いい', 'よい'],
          pos: 'AUX',
          dep: 'fixed',
          conjugationClass: '形容詞',
        },
        'ii'
      );

      // Require structural relationships
      // de --mark--> verb
      b2.headChild(verb, de, 'mark');
      // mo --fixed--> de
      b2.headChild(de, mo, 'fixed');
      // ii --fixed--> de
      b2.headChild(de, ii, 'fixed');

      b2.inOrder(verb, de);
      b2.inOrder(de, mo, 1);
      b2.inOrder(mo, ii, 1);

      // Capture the full pattern
      b2.captureSpan('verb-てもいい', verb, ii);
    }
  );
});
