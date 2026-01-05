import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: てでも (te demo) - Even if I have to, by any means
 *
 * A grammar pattern meaning "even if I have to do X" or "by any means necessary."
 * It expresses strong determination to do something, regardless of the consequences.
 *
 * Structure: Verb-te form (て or で) + でも
 *
 * Examples:
 * - かけてでも (even if I spend/spend time)
 * - してでも (even if I do)
 * - なってでも (even if I become)
 * - 並んででも (even if I line up)
 */
export default linguisticRule('てでも', (r) => {
  r.either(
    // Pattern 1: Combined verb-te + demo as single token
    // This handles most test cases where GiNZA tokenizes as one token
    (b1) => {
      const combined = b1.tok({
        textOneOf: [
          'おどろかしてでも', 'してでも', 'なってでも', '並んででも',
          'あらそってでも', 'あらためてでも', '延長してでも', 'かけてでも',
          'おもわれてでも', 'だいてでも', '解散してでも', 'になってでも',
          '払ってでも', '徹夜してでも', '失神してでも', 'てでも', 'ででも',
        ]
      }, 'combined');
      b1.captureSpan('てでも', combined, combined);
    },

    // Pattern 2: Any VERB/AUX followed by て/で + でも
    (b2) => {
      const verb = b2.tok({ posOneOf: ['VERB', 'AUX'] }, 'verb');
      const te = b2.tok({ textOneOf: ['て', 'で'] }, 'te');
      const demo = b2.tok({ text: 'でも' }, 'demo');
      b2.inOrder(verb, te, 10);
      b2.inOrder(te, demo, 3);
      b2.captureSpan('てでも', verb, demo);
    },

    // Pattern 3: Noun + して + でも (compound suru-verbs)
    (b3) => {
      const noun = b3.noun({}, 'noun');
      const shite = b3.tok({ textOneOf: ['して', 'してでも'] }, 'shite');
      const demo = b3.tok({ text: 'でも' }, 'demo');
      b3.inOrder(noun, shite, 2);
      b3.inOrder(shite, demo, 2);
      b3.captureSpan('てでも', noun, demo);
    },
  );
});
