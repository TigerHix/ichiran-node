import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てよかった (verb/adjective[te] + よかった)
 *
 * Expresses relief or gratitude that something happened or didn't happen.
 * "I'm glad that...", "It was good that..."
 *
 * Examples:
 * - してよかった (I'm glad I did it)
 * - きてよかった (I'm glad I came)
 * - 買わなくてよかった (I'm glad I didn't buy it)
 * - 間に合ってよかったです (I'm glad I made it in time)
 *
 * Key characteristics:
 * - Te-form verb/adj + よかった (past of よい/good)
 * - Expresses relief about past events
 * - Can be positive (did something) or negative (didn't do something)
 * - Both casual (よかった) and polite (よかったです) forms
 *
 * Grammar structure:
 * - Verb/Adj in te-form (連用形 + て/で SCONJ)
 * - よかった adjective stem (よかっ) + た (AUX)
 * - Optional: です (AUX) for polite form
 *
 * Negative forms:
 * - なくてよかった (glad didn't happen - formal)
 * - ないでよかった (glad didn't happen - casual)
 *
 * GiNZA parses よかった as:
 * - よかっ (VERB/ADJ, tag=形容詞-一般, conjugationClass=形容詞, inflectionForm=連用形-促音便)
 * - た (AUX, lemma=た, tag=助動詞, conjugationClass=助動詞-タ, dep=aux)
 */
export default bunproLinguisticRule('てよかった', (r) => {
  // Either casual or polite form
  r.either(
    // Pattern 1: Casual form with て/で particle (〜てよかった / 〜でよかった)
    // Matches verb/adj in te-form + よかった
    // e.g., してよかった, きてよかった, 間に合ってよかった
    (b1) => {
      const teForm = b1.tok({
        textOneOf: ['て', 'で'],
        pos: 'SCONJ',
      }, 'teForm');
      // GiNZA parses よかった as adjective stem + た auxiliary
      const yoiStem = b1.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-促音便',
      }, 'yoiStem');
      const ta = b1.aux({ lemma: 'た' }, 'ta');

      b1.inOrder(teForm, yoiStem, 1);
      b1.inOrder(yoiStem, ta, 1);
      b1.auxOf(yoiStem, ta);
      b1.captureSpan('てよかった', teForm, ta);
    },

    // Pattern 2: Polite form with て/で particle (〜てよかったです / 〜でよかったです)
    // e.g., してよかったです, 間に合ってよかったです
    (b2) => {
      const teForm = b2.tok({
        textOneOf: ['て', 'で'],
        pos: 'SCONJ',
      }, 'teForm');
      // GiNZA parses よかった as adjective stem + た auxiliary
      const yoiStem = b2.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-促音便',
      }, 'yoiStem');
      const ta = b2.aux({ lemma: 'た' }, 'ta');
      const desu = b2.aux({ lemma: 'です' }, 'desu');

      b2.inOrder(teForm, yoiStem, 1);
      b2.inOrder(yoiStem, ta, 1);
      b2.inOrder(ta, desu, 1);
      b2.auxOf(yoiStem, ta);
      b2.auxOf(ta, desu);
      b2.captureSpan('てよかった', teForm, desu);
    }
  );
});
