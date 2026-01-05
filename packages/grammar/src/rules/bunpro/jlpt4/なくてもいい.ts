import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: なくてもいい (Don't have to do.../It's okay not to do...)
 *
 * Matches negative verb て-form (なくて) + も + いい (+ です),
 * meaning "don't have to do", "it's okay not to do", "no need to do" (permission/optionality)
 *
 * This is the opposite of てもいい - instead of permission to do something,
 * it expresses that not doing something is also acceptable.
 *
 * GiNZA parses this as:
 * - verb (dictionary form)
 * - なく (AUX, lemma: ない) - negative auxiliary in te-form
 * - て (SCONJ) - te-form marker
 * - も (ADP) - emphasis particle
 * - いい (AUX/ADJ, lemma: いい) - good/okay
 *
 * Examples:
 * - 行かなくてもいい (don't have to go - casual)
 * - 行かなくてもいいです (don't have to go - polite)
 * - しなくてもいい (don't have to do - casual)
 * - 飲まなくてもいい (don't have to drink - casual)
 *
 * Handles both casual and polite forms (with です)
 * The particle も can be omitted in casual speech (なくていい)
 *
 * This rule should NOT match:
 * - Positive てもいい forms (e.g., 行ってもいい - may go)
 * - Simple negative なくて without もいい (e.g., 行かなくて - just negative te-form)
 * - ないで forms (different negative pattern)
 */
export default bunproLinguisticRule('なくてもいい', (r) => {
  r.either(
    // Branch 1: Verb + なく + て + も + いい (e.g., 行かなくてもいい)
    (b1) => {
      const verb = b1.verb({}, 'verb');

      const naku = b1.aux(
        {
          text: 'なく',
          lemma: 'ない',
        },
        'naku'
      );

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
          posOneOf: ['AUX', 'ADJ'],
          dep: 'fixed',
        },
        'ii'
      );

      // Require structural relationships
      // naku --aux--> verb (negative auxiliary)
      b1.auxOf(verb, naku);
      // te --mark--> verb (te-form connects to verb, not naku)
      b1.headChild(verb, te, 'mark');
      // mo --fixed--> te
      b1.headChild(te, mo, 'fixed');
      // ii --fixed--> te
      b1.headChild(te, ii, 'fixed');

      b1.inOrder(verb, naku, 1);
      b1.inOrder(naku, te, 1);
      b1.inOrder(te, mo, 1);
      b1.inOrder(mo, ii, 1);

      // Capture the full pattern
      b1.captureSpan('なくてもいい', verb, ii);
    },
    // Branch 1b: Verb + なく + て + いい (without も - casual speech)
    (b1b) => {
      const verb = b1b.verb({}, 'verb');

      const naku = b1b.aux(
        {
          text: 'なく',
          lemma: 'ない',
        },
        'naku'
      );

      const te = b1b.tok(
        {
          text: 'て',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'te'
      );

      const ii = b1b.tok(
        {
          lemmaOneOf: ['いい', 'よい'],
          posOneOf: ['AUX', 'ADJ'],
          dep: 'root',
        },
        'ii'
      );

      // Require structural relationships
      // naku --aux--> verb (negative auxiliary)
      b1b.auxOf(verb, naku);
      // te --mark--> verb (te-form connects to verb, not naku)
      b1b.headChild(verb, te, 'mark');
      // ii is the root of the sentence (when も is omitted)

      b1b.inOrder(verb, naku, 1);
      b1b.inOrder(naku, te, 1);
      b1b.inOrder(te, ii, 1);

      // Capture the full pattern
      b1b.captureSpan('なくてもいい', verb, ii);
    },
    // Branch 2: Verb + なく + て + も + いい + です (e.g., 行かなくてもいいです)
    (b2) => {
      const verb = b2.verb({}, 'verb');

      const naku = b2.aux(
        {
          text: 'なく',
          lemma: 'ない',
        },
        'naku'
      );

      const te = b2.tok(
        {
          text: 'て',
          pos: 'SCONJ',
          dep: 'mark',
        },
        'te'
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
          posOneOf: ['AUX', 'ADJ'],
          dep: 'fixed',
        },
        'ii'
      );

      const desu = b2.aux(
        {
          lemma: 'です',
        },
        'desu'
      );

      // Require structural relationships
      // naku --aux--> verb (negative auxiliary)
      b2.auxOf(verb, naku);
      // te --mark--> verb (te-form connects to verb, not naku)
      b2.headChild(verb, te, 'mark');
      // mo --fixed--> te
      b2.headChild(te, mo, 'fixed');
      // ii --fixed--> te
      b2.headChild(te, ii, 'fixed');
      // desu --aux/cop--> ii or verb
      b2.either(
        (b2a) => {
          b2a.headChild(ii, desu, 'cop');
        },
        (b2b) => {
          b2b.auxOf(verb, desu);
        }
      );

      b2.inOrder(verb, naku, 1);
      b2.inOrder(naku, te, 1);
      b2.inOrder(te, mo, 1);
      b2.inOrder(mo, ii, 1);
      b2.inOrder(ii, desu, 3);

      // Capture the full pattern
      b2.captureSpan('なくてもいい', verb, desu);
    }
  );
});
