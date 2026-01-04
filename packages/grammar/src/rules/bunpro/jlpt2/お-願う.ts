import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: お-願う (o-negau - humble request pattern)
 *
 * Pattern: Honorific prefix お/ご + Verb-masu stem/noun + 願う (to request/wish)
 *
 * This is a humble speech expression used for making polite requests.
 * Similar to お-ください but even more formal.
 *
 * Examples:
 * - お待ち願います (Please wait)
 * - ご確認願います (Please check/confirm)
 * - お返事願います (Please reply)
 * - ご検討願います (Please consider)
 *
 * EXCLUDES:
 * - お願いします - common "please" phrase (different grammar)
 *
 * GiNZA parse variations:
 * - Kanji form: お/ご + stem + 願う (VERB/AUX) + ます (AUX)
 * - Merged compounds: お察し願います (single compound + auxiliaries)
 *
 * TODO: Hiragana forms like "おまちねがいます" need additional patterns
 */
export default linguisticRule('お-願う', (r) => {
  r.either(
    // Pattern 1: お/ご prefix + stem + 願う (kanji form)
    // Matches: お待ち願います, ご確認願います
    (b) => {
      const prefix = b.tok({
        textOneOf: ['お', 'ご'],
        pos: 'NOUN',
        dep: 'compound'
      }, 'prefix');

      const stem = b.tok({
        posOneOf: ['VERB', 'NOUN'],
      }, 'stem');

      const negau = b.tok({
        lemma: '願う',
      }, 'negau');

      b.inOrder(prefix, stem, 1);
      b.inOrder(stem, negau, 3);

      // Only match if stem is NOT "願い" (to exclude "お願いします")
      b.not((nb) => {
        const onegaiStem = nb.tok({
          text: '願い',
          posOneOf: ['VERB', 'NOUN'],
        });
        nb.inOrder(prefix, onegaiStem, 1);
        nb.inOrder(onegaiStem, negau, 3);
      });

      b.captureSpan('お-願う', prefix, negau);
    },

    // Pattern 2: Token starting with お/ご + 願う (merged compounds, kanji)
    // Examples: お察し願います
    (b) => {
      const compound = b.tok({
        textOneOf: ['お'], // For indexing
        textRe: /^(お|ご)/,
        posOneOf: ['NOUN', 'VERB'],
      }, 'compound');

      const negau = b.tok({
        lemma: '願う',
      }, 'negau');

      b.inOrder(compound, negau, 5);

      // Compound must NOT be exactly "お願い"
      b.not((nb) => {
        const onegaiCompound = nb.tok({
          text: 'お願い',
          posOneOf: ['NOUN', 'VERB'],
        });
        nb.inOrder(onegaiCompound, negau, 5);
      });

      b.captureSpan('お-願う', compound, negau);
    },

    // Pattern 3: Token starting with お/ご + token ending in ねがいます (hiragana form)
    // Examples: おまちねがいます, ごかくにんねがいます
    (b) => {
      const compound = b.tok({
        textOneOf: ['お'], // For indexing
        textRe: /^(お|ご)/,
        posOneOf: ['NOUN', 'VERB'],
      }, 'compound');

      const negau = b.tok({
        textRe: /ねがいます$/,
      }, 'negau');

      b.inOrder(compound, negau, 5);

      b.captureSpan('お-願う', compound, negau);
    }
  );
});
