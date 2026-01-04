import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からして (karashite) - Even..., Judging from, Based on
 *
 * A phrase used when the speaker judges from (A) or gives their opinion
 * based on (A). Indicates that (A) is the starting point or basis for
 * a judgment, often with emphasis.
 *
 * Structure: Noun + からして
 *
 * Examples:
 * - 彼の性格からして、彼と一緒に住むことは無理だろう。
 *   (Judging from his personality, it's probably impossible to live with him.)
 * - 値段からして、このお店は新鮮な食材を使っているに違いない。
 *   (Based on the price, this restaurant must be using fresh ingredients.)
 * - このゲームは名前からしてつまらなそうだ。
 *   (Judging from the name, this game seems boring.)
 * - 親からして反対している。
 *   (Even the parents are opposed.)
 *
 * Key discriminators:
 * - Follows nouns (NOUN, PROPN, PRON)
 * - から is a particle (ADP/SCONJ) indicating "from"
 * - して is the te-form of する (VERB/AUX)
 * - Expresses judgment based on a single piece of evidence
 * - Similar to からすると・からすれば but more subjective/emphatic
 *
 * GiNZA parse structure:
 * - NOUN + から(ADP/SCONJ) + して(VERB/AUX)
 * - Various dependency relations (compound, fixed, mark)
 *
 * Different from:
 * - からして as "even starting with" (emphatic)
 * - から alone as "because" or "from"
 * - にしては ("considering, for")
 * - からすると (more objective judgment)
 */
export default linguisticRule('からして', (r) => {
  r.either(
    // Pattern 1: Noun + から(ADP) + して(AUX) with compound dependency
    // Most common pattern: NOUN is head, から and して form compound
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kara = b1.particle('から', 'kara', { pos: 'ADP' });
      const shite = b1.aux({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shite');

      b1.inOrder(noun, kara, 1);
      b1.inOrder(kara, shite, 1);
      b1.headChild(noun, kara, 'compound');
      b1.headChild(noun, shite, 'compound');

      b1.captureSpan('からして', noun, shite);
    },

    // Pattern 2: Noun + から(ADP) + して(VERB) with compound dependency
    // Alternative parsing where して is tagged as VERB
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kara = b2.particle('から', 'kara', { pos: 'ADP' });
      const shite = b2.verb({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shite');

      b2.inOrder(noun, kara, 1);
      b2.inOrder(kara, shite, 1);
      b2.headChild(noun, kara, 'compound');
      b2.headChild(noun, shite, 'compound');

      b2.captureSpan('からして', noun, shite);
    },

    // Pattern 3: Noun + から(SCONJ) + して with fixed dependency
    // SCONJ is used for conjunctive/coordinate particles
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kara = b3.particle('から', 'kara', { pos: 'SCONJ' });
      const shite = b3.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shite');

      b3.inOrder(noun, kara, 1);
      b3.inOrder(kara, shite, 1);
      b3.headChild(noun, kara, 'fixed');
      b3.headChild(noun, shite, 'fixed');

      b3.captureSpan('からして', noun, shite);
    },

    // Pattern 4: Noun + から + して with mark dependency
    // から as clause marker
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kara = b4.particle('から', 'kara');
      const shite = b4.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shite');

      b4.inOrder(noun, kara, 1);
      b4.inOrder(kara, shite, 1);
      b4.headChild(noun, kara, 'mark');
      b4.headChild(noun, shite, 'mark');

      b4.captureSpan('からして', noun, shite);
    },

    // Pattern 5: Noun + から + して (catch-all with loose dependency)
    // For unexpected GiNZA parsings
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kara = b5.particle('から', 'kara');
      const shite = b5.tok({ lemma: 'する', inflectionForm: '連用形-一般' }, 'shite');

      b5.inOrder(noun, kara, 1);
      b5.inOrder(kara, shite, 1);

      b5.captureSpan('からして', noun, shite);
    }
  );
});
