import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: だけましだ (dake mashi da) - "at least, it's better than, one should feel grateful for"
 *
 * An expression used to say that while the situation is not ideal, at least
 * this one good thing exists, or it could be worse. Combines "dake" (only/just)
 * with "mashi" (better), expressing gratitude that things aren't worse.
 *
 * Structures:
 * - Verb + だけましだ / だけましです
 * - I-adjective + だけましだ / だけましです
 * - Na-adjective + な + だけましだ / だけましです
 * - Noun + である + だけましだ / だけましです
 *
 * Often used without copula: Verb/Adj + だけまし
 *
 * Examples:
 * - 久しぶりに旅行に来れただけましだ
 *   (At least I was able to travel for the first time in a long time.)
 * - 職場に近いだけましだ
 *   (At least it's close to my workplace.)
 * - 元気なだけましだ
 *   (At least I'm healthy.)
 * - 金庫が無事なだけましだ
 *   (At least the safe is intact.)
 *
 * Key discriminators:
 * - だけ (dake) is the particle meaning "only/just"
 * - まし (mashi) is from the verb 増す (to increase/better)
 * - まし acts as a na-adjective meaning "better" or "preferable"
 * - Optional だ/です copula
 * - Often used without copula: 〜だけまし
 *
 * GiNZA parse structure:
 * - "来れただけましだ" → 来れる(VERB) + だけ(ADP) + まし(ADJ) + だ(AUX)
 * - "近いだけましだ" → 近い(ADJ) + だけ(ADP) + まし(ADJ) + だ(AUX)
 * - "元気なだけましだ" → 元気(NOUN/ADJ) + な(ADP) + だけ(ADP) + まし(ADJ) + だ(AUX)
 * - "無事なだけましだ" → 無事(NOUN/ADJ) + な(ADP) + だけ(ADP) + まし(ADJ) + だ(AUX)
 */
export default bunproLinguisticRule('だけましだ', (r) => {
  r.either(
    // Pattern 1: With copula だ (casual)
    // Verb/I-adj + だけましだ OR Na-adj/Noun + な + だけましだ
    (b1) => {
      const dake = b1.tok({ lemma: 'だけ' }, 'dake');
      const mashi = b1.tok({ lemma: 'まし', posOneOf: ['ADJ', 'NOUN'] }, 'mashi');
      const da = b1.aux({ lemmaOneOf: ['だ', 'です'] }, 'da');

      b1.inOrder(dake, mashi, 2);  // Allow optional な between
      b1.inOrder(mashi, da, 1);
      b1.captureSpan('だけましだ', dake, da);
    },

    // Pattern 2: Without copula (often used this way)
    // Verb/I-adj + だけまし OR Na-adj/Noun + な + だけまし
    (b2) => {
      const dake = b2.tok({ lemma: 'だけ' }, 'dake');
      const mashi = b2.tok({ lemma: 'まし', posOneOf: ['ADJ', 'NOUN'] }, 'mashi');

      b2.inOrder(dake, mashi, 2);  // Allow optional な between
      b2.captureSpan('だけまし', dake, mashi);
    }
  );
});
