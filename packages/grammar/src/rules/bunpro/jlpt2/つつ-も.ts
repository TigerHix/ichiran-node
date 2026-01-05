import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: つつ(も) (tsutsu mo) - "Even while ~ing, Despite ~ing"
 *
 * A formal grammar pattern expressing that "even while (A), (B)". Despite (A) being
 * the underlying principle, (B) is contradictory. Similar to ながらも but more formal
 * and literary.
 *
 * Structure:
 * - Verb stem (masu form minus ます) + つつ(も)
 * - も is optional - both つつ and つつも are acceptable
 *
 * Examples:
 * - 知りつつも、無視した。
 *   (Despite knowing, I ignored it.)
 * - 緊張しつつも、楽しめました。
 *   (Despite being nervous, I was able to enjoy it.)
 * - 彼の言葉は怪しいと思いつつも、信じてみた。
 *   (Even though I thought his words were suspicious, I tried to believe him.)
 * - 実力不足を実感しつつも、プロを諦められません。
 *   (Despite feeling that I lack the ability, I still won't give up being a professional.)
 *
 * Key discriminators:
 * - The "つつ" form attaches to the verb stem (連用形)
 * - "も" is optional but adds concessive meaning ("even while")
 * - More formal than ながらも
 * - Used primarily for non-physical actions (thinking, knowing, feeling)
 * - Expresses contradiction between (A) and (B)
 *
 * GiNZA parse structure:
 * - Verb stem (連用形) with inflectionForm=連用形-一般
 * - つつ as AUX or PART
 * - も as PART (optional)
 *
 * Different from:
 * - ながら (while) - less formal, used with physical actions
 * - ながらも (while/even though) - less formal, can be used with more verb types
 * - くせに (despite) - more critical/blaming tone
 */
export default bunproLinguisticRule('つつ-も', (r) => {
  r.either(
    // Pattern 1: Verb stem + つつ + も (split tokens)
    // e.g., 知りつつも、なりつつも、思いつつも
    (b1) => {
      const verb = b1.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const tsutsu = b1.tok({ text: 'つつ', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tsutsu');
      const mo = b1.particle('も', 'mo');

      b1.inOrder(verb, tsutsu, 1);
      b1.inOrder(tsutsu, mo, 1);

      b1.captureSpan('つつ-も', verb, mo);
    },

    // Pattern 2: Verb stem + つつも (single combined token)
    // Sometimes つつも is parsed as one token
    (b2) => {
      const verb = b2.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const tsutsumo = b2.tok({ text: 'つつも', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tsutsumo');

      b2.inOrder(verb, tsutsumo, 1);

      b2.captureSpan('つつ-も', verb, tsutsumo);
    },

    // Pattern 3: Verb stem + つつ (without も)
    // Both つつ and つつも are acceptable for this grammar point
    (b3) => {
      const verb = b3.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const tsutsu = b3.tok({ text: 'つつ', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tsutsu');

      b3.inOrder(verb, tsutsu, 1);

      b3.captureSpan('つつ-も', verb, tsutsu);
    },

    // Pattern 4: More flexible verb matching (any verb form that can attach to つつも)
    // Some verbs may not have the inflectionForm marked
    (b4) => {
      const verb = b4.verb({}, 'verb');
      const tsutsu = b4.tok({ text: 'つつ', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tsutsu');
      const mo = b4.particle('も', 'mo');

      b4.inOrder(verb, tsutsu, 2);
      b4.inOrder(tsutsu, mo, 2);

      b4.captureSpan('つつ-も', verb, mo);
    },

    // Pattern 5: Very flexible - verb + つつも combined
    (b5) => {
      const verb = b5.verb({}, 'verb');
      const tsutsumo = b5.tok({ text: 'つつも', posOneOf: ['AUX', 'PART', 'SCONJ', 'ADP'] }, 'tsutsumo');

      b5.inOrder(verb, tsutsumo, 2);

      b5.captureSpan('つつ-も', verb, tsutsumo);
    },

    // Pattern 6: Verb + つつ (without も, flexible matching)
    (b6) => {
      const verb = b6.verb({}, 'verb');
      const tsutsu = b6.tok({ text: 'つつ', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tsutsu');

      b6.inOrder(verb, tsutsu, 2);

      b6.captureSpan('つつ-も', verb, tsutsu);
    },

    // Pattern 7: Catch-all - verb followed by つつも
    // Handles various GiNZA tokenization patterns
    (b7) => {
      const verb = b7.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const tsutsumo = b7.tok({ text: 'つつも', posOneOf: ['AUX', 'PART', 'SCONJ', 'ADP'] }, 'tsutsumo');

      b7.inOrder(verb, tsutsumo, 3);

      b7.captureSpan('つつ-も', verb, tsutsumo);
    },

    // Pattern 8: Split tokens - verb + つつ + も (very flexible)
    (b8) => {
      const verb = b8.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const tsutsu = b8.tok({ text: 'つつ', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tsutsu');
      const mo = b8.particle('も', 'mo');

      b8.inOrder(verb, tsutsu, 3);
      b8.inOrder(tsutsu, mo, 3);

      b8.captureSpan('つつ-も', verb, mo);
    },

    // Pattern 9: Catch-all - verb + つつ (without も)
    (b9) => {
      const verb = b9.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const tsutsu = b9.tok({ text: 'つつ', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'tsutsu');

      b9.inOrder(verb, tsutsu, 3);

      b9.captureSpan('つつ-も', verb, tsutsu);
    }
  );
});
