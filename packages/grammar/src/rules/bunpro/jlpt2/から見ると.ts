import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: から見ると (kara miru to) - "From the point of view of, By the look of"
 *
 * A hypothetical expression that points out how something looks from the
 * perspective of (A). Used to judge or evaluate something from a particular
 * standpoint or based on appearance.
 *
 * Structure:
 * - Noun + から見ると (kara miru to) - "judging from the perspective of"
 * - Noun + から見れば (kara mirereba) - "from the standpoint of"
 * - Noun + から見て (kara mite) - "by the look of, based on"
 * - Noun + から見たら (kara mitara) - "if one looks from"
 *
 * Also accepts hiragana variants: からみる, からみれば, からみて, からみたら
 *
 * Examples:
 * - 専門家から見ると、この問題は簡単だ。
 *   (From an expert's point of view, this problem is simple.)
 * - 外国人から見れば、日本の習慣は奇妙に見える。
 *   (From a foreigner's standpoint, Japanese customs seem strange.)
 * - 子供から見て、この値段は高すぎる。
 *   (From a child's perspective, this price is too high.)
 * - 親から見たら、心配なのは当然だ。
 *   (If you look from a parent's perspective, it's natural to worry.)
 *
 * Key discriminators:
 * - Follows a noun (NOUN, PROPN, PRON) indicating the perspective
 * - Uses から (kara) particle meaning "from"
 * - Uses 見る (miru) verb meaning "to see/look/judge"
 * - Conditional endings (と/ば/て/たら) indicate hypothetical evaluation
 * - Expresses objective judgment from a specific viewpoint
 *
 * GiNZA parse structure:
 * - NOUN + から(ADP) + 見る(VERB) + conditional(と/ば/AUX/etc.)
 *
 * Different from:
 * - からして (more subjective/emphatic judgment)
 * - からすると・からすれば (slightly more objective, using する)
 * - にしたら (personal/subjective perspective)
 * - Simple 見る (to see/watch - without perspective marker)
 */
export default linguisticRule('から見ると', (r) => {
  // Preceding noun (perspective/standpoint)
  const noun = r.tok({
    posOneOf: ['NOUN', 'PROPN', 'PRON'],
  }, 'noun');

  // Particle から (from)
  const kara = r.particle('から', 'kara');
  r.inOrder(noun, kara, 1);

  r.either(
    // Pattern 1: から見ると (kara miru to) - "judging from the perspective of"
    // 見る(VERB) + と(ADP/SCONJ) as conditional marker
    // Accept both hiragana (みる) and kanji (見る) variants
    (b1) => {
      const miru = b1.tok({ lemmaOneOf: ['見る', 'みる'] }, 'miru');
      const to = b1.tok({ text: 'と' }, 'to');
      b1.inOrder(kara, miru, 1);
      b1.inOrder(miru, to, 1);
      b1.captureSpan('から見ると', noun, to);
    },

    // Pattern 2: から見れば (kara mirereba) - "from the standpoint of"
    // 見れ(VERB, 仮定形) + ば(SCONJ) as conditional marker
    (b2) => {
      const mire = b2.verb({
        lemmaOneOf: ['見る', 'みる'],
        inflectionForm: '仮定形-一般',
      }, 'mire');
      const ba = b2.tok({
        text: 'ば',
        pos: 'SCONJ',
      }, 'ba');
      b2.inOrder(kara, mire, 1);
      b2.inOrder(mire, ba, 1);
      b2.captureSpan('から見ると', noun, ba);
    },

    // Pattern 3: から見て (kara mite) - "by the look of, based on"
    // 見て(VERB/AUX, 連用形) as te-form
    (b3) => {
      const mite = b3.tok({
        lemmaOneOf: ['見る', 'みる'],
        inflectionForm: '連用形-一般',
      }, 'mite');
      b3.inOrder(kara, mite, 1);
      b3.captureSpan('から見ると', noun, mite);
    },

    // Pattern 4: から見たら (kara mitara) - "if one looks from"
    // 見た(VERB/AUX, 連用形) + ら(AUX, 仮定形) as tara conditional
    (b4) => {
      const mita = b4.tok({
        lemmaOneOf: ['見る', 'みる'],
        inflectionForm: '連用形-一般',
      }, 'mita');
      const ra = b4.aux({
        text: 'ら',
        inflectionForm: '仮定形-一般',
      }, 'ra');
      b4.inOrder(kara, mita, 1);
      b4.inOrder(mita, ra, 1);
      b4.captureSpan('から見ると', noun, ra);
    }
  );
});
