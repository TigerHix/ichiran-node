import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: てもらう (te-morau) - to have someone do something for you
 *
 * Verb[te-form] + もらう expresses receiving the favor of someone doing an action.
 * Unlike てあげる (doing for someone), てもらう focuses on the receiver's perspective.
 *
 * Examples:
 * - いれてもらった (had someone make [coffee] for me)
 * - してもらう (will have someone do for me)
 * - 読んでもらいます (will have someone read for me - polite)
 *
 * The receiver is marked with に, the doer is the subject.
 *
 * GiNZA parsing notes:
 * - Verb-te-forms are parsed as: verb stem + て/で (SCONJ)
 * - Regular verbs use て (lemma=て), ん-declination uses で (lemma=で)
 * - て/で has pos=SCONJ, attaching to the verb stem
 * - もらう is the main verb (pos=VERB, lemma=もらう)
 *
 * Forms handled:
 * - Present: てもらう
 * - Past: てもらった
 * - Te-form: てもらって
 * - Polite: てもらいます, てもらいました
 * - Humble (尊敬): ていただく, ていただいた, ていただいて
 */
export default linguisticRule('てもらう', (r) => {
  r.either(
    // Pattern 1: Present/past forms (てもらう, てもらった)
    // Example: いれてもらった, してもらう, かってもらう, よんでもらう
    // GiNZA: verb + て/で (SCONJ) + もらう (VERB)
    (b) => {
      const te = b.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const morau = b.verb({ lemma: 'もらう' }, 'morau');

      b.inOrder(te, morau, 1);
      b.captureSpan('てもらう', te, morau);
    },

    // Pattern 2: Te-form (てもらって)
    // Example: してもらって, もっていってもらって, よんでもらって
    // GiNZA: verb + て/で (SCONJ) + もら (lemma) + て/で (SCONJ)
    (b) => {
      const te1 = b.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te1');
      const mora = b.tok({ lemma: 'もらう' }, 'mora');
      const te2 = b.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te2');

      b.inOrder(te1, mora, 1);
      b.inOrder(mora, te2, 1);
      b.captureSpan('てもらう', te1, te2);
    },

    // Pattern 3: Polite forms (てもらいます, てもらいました)
    // Example: よんでもらいます, していってもらいましょう
    // GiNZA: verb + て/で (SCONJ) + もらいます (VERB, lemma=もらう)
    (b) => {
      const te = b.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const morau = b.verb({
        lemma: 'もらう',
        inflectionForm: '連用形-一般',
      }, 'morau');

      b.inOrder(te, morau, 2);  // Allow distance for polite auxiliaries
      b.captureSpan('てもらう', te, morau);
    },

    // Pattern 4: Humble/Respectful forms (ていただく, ていただいた, ていただいて)
    // Example: いれていただいた, していただく, よんでもらう
    // GiNZA: verb + て/で (SCONJ) + いただく (VERB, lemma=いただく)
    (b) => {
      const te = b.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      const itadaku = b.verb({ lemma: 'いただく' }, 'itadaku');

      b.inOrder(te, itadaku, 1);
      b.captureSpan('てもらう', te, itadaku);
    },

    // Pattern 5: Humble te-form (ていただいて)
    // Example: していただいて, よんでもらって
    // GiNZA: verb + て/で (SCONJ) + いただい (lemma) + て/で (SCONJ)
    (b) => {
      const te1 = b.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te1');
      const itada = b.tok({ lemma: 'いただく' }, 'itada');
      const te2 = b.tok({ lemmaOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te2');

      b.inOrder(te1, itada, 1);
      b.inOrder(itada, te2, 1);
      b.captureSpan('てもらう', te1, te2);
    }
  );
});
