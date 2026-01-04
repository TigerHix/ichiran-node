import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: から見ると (karamiruto) - "From the standpoint of, By the look of"
 *
 * A hypothetical expression that points out how something looks from the perspective of (A).
 * Used for objective judgment from a certain perspective, often based on observable facts.
 *
 * Structure:
 * - Noun + から見ると (kara + miru + to)
 * - Noun + から見たら (kara + mira + ta)
 * - Noun + から見れば (kara + mirure + ba)
 * - Noun + から見て (kara + mite)
 *
 * Examples:
 * - 素人から見るとかなりうまい人でも、プロの世界では全然通用しないらしい。
 *   (From a layperson's point of view, even people who are considerably skilled would not get by in the pro world.)
 * - 外から見ると、やりやすそうです。
 *   (From an outside point of view it looks easy.)
 * - 私から見れば、彼女はあまり美人じゃない。
 *   (From my point of view, she is not that much of a stunner.)
 * - このタイヤ跡の大きさから見て、犯人はトラックを使ったに違いない。
 *   (By the looks of the size of these tire tracks, the perpetrator must have used a truck.)
 *
 * Key discriminators:
 * - Follows nouns (NOUN, PROPN, PRON) - the perspective/standpoint
 * - から is a particle (ADP/SCONJ) indicating "from"
 * - 見る/みる (miru) is the verb "to see/look/judge"
 * - Followed by conditional form (と/たら/ば) or te-form (て)
 * - Expresses judgment/inference from a given perspective
 *
 * Different from:
 * - からして (more subjective/emphatic judgment)
 * - からすると・からすれば (more objective judgment)
 * - にしたら (emphasizes personal, subjective perspective)
 * - にしては (highlights result departing from expectations)
 *
 * GiNZA parse structure:
 * - NOUN + から(ADP/SCONJ) + 見る/みる(VERB) + conditional particle
 * - Note: The verb can be in hiragana (みる) or kanji (見る)
 */
export default linguisticRule('から見ると', (r) => {
  // Preceding noun (the perspective/standpoint)
  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'PRON'],
  }, 'noun');

  // Particle から (from)
  const kara = r.particle('から', 'kara');
  r.inOrder(noun, kara, 1);

  r.either(
    // Pattern 1: から + みる/見る + と (から見ると/からみると)
    (b1) => {
      const miru = b1.verb({ lemmaOneOf: ['みる', '見る'] }, 'miru');
      const to = b1.tok({ text: 'と' }, 'to');
      b1.inOrder(kara, miru, 5);
      b1.inOrder(miru, to, 1);
      b1.captureSpan('から見ると', noun, to);
    },

    // Pattern 2: から + み + たら (から見たら/からみたら)
    (b2) => {
      const mira = b2.tok({ textOneOf: ['見', 'み'], lemmaOneOf: ['見る', 'みる'] }, 'mira');
      const tara = b2.tok({ text: 'たら' }, 'tara');
      b2.inOrder(kara, mira, 5);
      b2.inOrder(mira, tara, 1);
      b2.captureSpan('から見ると', noun, tara);
    },

    // Pattern 3: から + みれ + ば (から見れば/からみれば)
    (b3) => {
      const mirure = b3.tok({ textOneOf: ['見れ', 'みれ'], lemmaOneOf: ['見る', 'みる'] }, 'mirure');
      const ba = b3.tok({ text: 'ば', pos: 'SCONJ' }, 'ba');
      b3.inOrder(kara, mirure, 5);
      b3.inOrder(mirure, ba, 1);
      b3.captureSpan('から見ると', noun, ba);
    },

    // Pattern 4: から + みて (から見て/からみて) - te-form
    (b4) => {
      const mite = b4.aux({
        lemmaOneOf: ['みる', '見る'],
        inflectionForm: '連用形-一般'
      }, 'mite');
      b4.inOrder(kara, mite, 5);
      b4.captureSpan('から見ると', noun, mite);
    }
  );
});
