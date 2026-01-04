import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ばよかった (Verb[ba] + よかった)
 *
 * Expresses regret about not doing something.
 * "Should have done X", "I wish I had done X"
 *
 * Examples:
 * - 行けばよかった (I should have gone)
 * - すればよかった (I should have done it)
 * - 買えばよかった (I should have bought it)
 * - 待てばよかったです (I should have waited - polite)
 *
 * Key characteristics:
 * - Verb in ba-form (仮定形-一般)
 * - Followed by よかった (past form of いい)
 * - Optional: です for polite form
 * - Expresses regret about past actions NOT taken
 *
 * Grammar structure:
 * - Verb in conditional form (仮定形-一般) ending in ば
 * - よかった (past tense of いい)
 * - Optional: です for politeness
 *
 * GiNZA parses:
 * - Verbs in ba-form can be either:
 *   1. Single token: VERB(lemma=X, inflectionForm=仮定形-一般) - e.g., 行けば
 *   2. Two tokens: VERB(lemma=X, 仮定形-一般) + SCONJ(ば) - e.g., まてば
 * - よかった as: よかっ(VERB/ADJ, 連用形-促音便) + た(AUX, lemma=た)
 */
export default linguisticRule('ばよかった', (r) => {
  r.either(
    // Pattern 1a: Standard casual form - verb[ba] (single token) + よかった (as single token)
    // Some sentences parse よかった as a single ADJ token
    // e.g., 行けばよかった, すればよかった
    (b1a) => {
      const verb = b1a.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '仮定形-一般',
      }, 'verb');

      // Exclude tara-form (lemma=た with inflectionForm=仮定形-一般)
      b1a.not((nb) => {
        nb.tok({
          pos: 'AUX',
          lemma: 'た',
          inflectionForm: '仮定形-一般',
        }, 'verb');
      });

      const yokatta = b1a.tok({
        text: 'よかった',
        pos: 'ADJ',
      }, 'yokatta');

      b1a.inOrder(verb, yokatta, 2);
      b1a.captureSpan('ばよかった', verb, yokatta);
    },

    // Pattern 1b: Standard casual form - verb[ba] (single token) + よかった (split)
    // e.g., 行けばよかった, すればよかった, 買えばよかった
    // GiNZA: VERB(仮定形-一般) + よかった
    (b1) => {
      const verb = b1.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '仮定形-一般',
      }, 'verb');

      // Exclude tara-form (lemma=た with inflectionForm=仮定形-一般)
      b1.not((nb) => {
        nb.tok({
          pos: 'AUX',
          lemma: 'た',
          inflectionForm: '仮定形-一般',
        }, 'verb');
      });

      const yoiStem = b1.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-促音便',
      }, 'yoiStem');

      const ta = b1.aux({ lemma: 'た' }, 'ta');

      b1.inOrder(verb, yoiStem, 2);
      b1.inOrder(yoiStem, ta, 1);
      b1.auxOf(yoiStem, ta);
      b1.captureSpan('ばよかった', verb, ta);
    },

    // Pattern 2: Standard casual form - verb[ba] (verb + ば) + よかった
    // e.g., まてばよかった, かえばよかった, いけばよかった
    // GiNZA: VERB(仮定形-一般) + SCONJ(ば) + よかった
    (b2) => {
      const verb = b2.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '仮定形-一般',
      }, 'verb');

      const ba = b2.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      const yoiStem = b2.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-促音便',
      }, 'yoiStem');

      const ta = b2.aux({ lemma: 'た' }, 'ta');

      b2.inOrder(verb, ba, 1);
      b2.inOrder(ba, yoiStem, 2);
      b2.inOrder(yoiStem, ta, 1);
      b2.auxOf(yoiStem, ta);
      b2.captureSpan('ばよかった', verb, ta);
    },

    // Pattern 3: Polite form - verb[ba] (single token) + よかったです
    // e.g., 行けばよかったです, すればよかったです
    (b3) => {
      const verb = b3.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '仮定形-一般',
      }, 'verb');

      // Exclude tara-form (lemma=た with inflectionForm=仮定形-一般)
      b3.not((nb) => {
        nb.tok({
          pos: 'AUX',
          lemma: 'た',
          inflectionForm: '仮定形-一般',
        }, 'verb');
      });

      const yoiStem = b3.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-促音便',
      }, 'yoiStem');

      const ta = b3.aux({ lemma: 'た' }, 'ta');

      const desu = b3.aux({ lemma: 'です' }, 'desu');

      b3.inOrder(verb, yoiStem, 2);
      b3.inOrder(yoiStem, ta, 1);
      b3.inOrder(ta, desu, 1);
      b3.auxOf(yoiStem, ta);
      b3.auxOf(ta, desu);
      b3.captureSpan('ばよかった', verb, desu);
    },

    // Pattern 4: Polite form - verb[ba] (verb + ば) + よかったです
    // e.g., まてばよかったです, かえばよかったです
    (b4) => {
      const verb = b4.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '仮定形-一般',
      }, 'verb');

      const ba = b4.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');

      const yoiStem = b4.tok({
        posOneOf: ['VERB', 'ADJ'],
        inflectionForm: '連用形-促音便',
      }, 'yoiStem');

      const ta = b4.aux({ lemma: 'た' }, 'ta');

      const desu = b4.aux({ lemma: 'です' }, 'desu');

      b4.inOrder(verb, ba, 1);
      b4.inOrder(ba, yoiStem, 2);
      b4.inOrder(yoiStem, ta, 1);
      b4.inOrder(ta, desu, 1);
      b4.auxOf(yoiStem, ta);
      b4.auxOf(ta, desu);
      b4.captureSpan('ばよかった', verb, desu);
    }
  );
});
