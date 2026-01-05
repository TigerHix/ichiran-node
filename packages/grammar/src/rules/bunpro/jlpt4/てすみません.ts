import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てすみません (te-sumimasen) - I'm sorry for doing/will do
 *
 * Verb[te-form] + すみません expresses apology for doing something.
 * It can also be used with すいません as a variant.
 *
 * Examples:
 * - 遅れてすみません (sorry for being late)
 * - たべてすみません (sorry for eating)
 * - しなくてすみません (sorry for not doing)
 * - してすみませんでした (was sorry for doing)
 *
 * GiNZA parsing notes:
 * - Verb-te-forms are parsed as: verb stem + て (SCONJ)
 * - すみません is parsed as: すみ (VERB, lemma=すむ) + ませ (AUX, lemma=ます) + ん (AUX, lemma=ぬ)
 * - すみませんでした is parsed as: すみ + ませ + ん + でし (AUX, lemma=です) + た (AUX, lemma=た)
 *
 * Forms handled:
 * - Present: てすみません, てすいません
 * - Past: てすみませんでした, てすいませんでした
 * - Negative te-form: なくてすみません
 */
export default bunproLinguisticRule('てすみません', (r) => {
  r.either(
    // Pattern 1: てすみません (present)
    // Example: 遅れてすみません, たべてすみません
    // GiNZA: te + すみ (VERB) + ませ (AUX) + ん (AUX)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const sumi = b.verb({ lemma: 'すむ' }, 'sumi');
      const mase = b.aux({ lemma: 'ます' }, 'mase');
      const n = b.aux({ lemma: 'ぬ' }, 'n');

      b.inOrder(te, sumi, 1);
      b.inOrder(sumi, mase, 1);
      b.inOrder(mase, n, 1);
      b.captureSpan('てすみません', te, n);
    },

    // Pattern 2: てすみませんでした (past)
    // Example: してすみませんでした, なってすみませんでした
    // GiNZA: te + すみ + ませ + ん + でし (AUX) + た (AUX)
    (b) => {
      const te = b.tok({ lemma: 'て', pos: 'SCONJ' }, 'te');
      const sumi = b.verb({ lemma: 'すむ' }, 'sumi');
      const mase = b.aux({ lemma: 'ます' }, 'mase');
      const n = b.aux({ lemma: 'ぬ' }, 'n');
      const deshi = b.aux({ lemma: 'です' }, 'deshi');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      b.inOrder(te, sumi, 1);
      b.inOrder(sumi, mase, 1);
      b.inOrder(mase, n, 1);
      b.inOrder(n, deshi, 1);
      b.inOrder(deshi, ta, 1);
      b.captureSpan('てすみません', te, ta);
    }
  );
});
