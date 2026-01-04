import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('きっかけ', (r) => {
  // きっかけ - "opportunity, chance, trigger, turning point"
  // Patterns:
  // 1. Noun/Verb + をきっかけに (taking X as an opportunity)
  // 2. Noun/Verb + をきっかけにして (taking X as an opportunity, formal)
  // 3. Noun/Verb + をきっかけとして (taking X as an opportunity, formal)
  // 4. Noun/Verb + がきっかけで (X became the trigger)
  // 5. Noun/Verb + がきっかけになって (X became the trigger, continuous)
  // 6. Noun/Verb + がきっかけとなって (X became the trigger, continuous)
  // 7. Noun/Verb + がきっかけである (X is the trigger)
  // 8. きっかけは何ですか (what was the trigger)

  r.either(
    // Pattern 1: (noun/verb + の/こと) + をきっかけに
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const wo = b.particle('を', 'wo');
      const ni = b.particle('に', 'ni');
      b.inOrder(wo, kikkake, 1);
      b.inOrder(kikkake, ni, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, wo, 10);
      b.captureSpan('きっかけ', start, ni);
    },
    // Pattern 2: (noun/verb + の/こと) + をきっかけにして
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const wo = b.particle('を', 'wo');
      const ni = b.particle('に', 'ni');
      const shite = b.aux({ lemma: 'する' }, 'shite');
      b.inOrder(wo, kikkake, 1);
      b.inOrder(kikkake, ni, 1);
      b.inOrder(ni, shite, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, wo, 10);
      b.captureSpan('きっかけ', start, shite);
    },
    // Pattern 3: (noun/verb + の/こと) + をきっかけとして
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const wo = b.particle('を', 'wo');
      const toshite = b.tok({ text: 'として' }, 'toshite');
      b.inOrder(wo, kikkake, 1);
      b.inOrder(kikkake, toshite, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, wo, 10);
      b.captureSpan('きっかけ', start, toshite);
    },
    // Pattern 3b: (noun/verb + の/こと) + をきっかけと + して (two tokens)
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const wo = b.particle('を', 'wo');
      const to = b.particle('と', 'to');
      const shite = b.tok({ text: 'して' }, 'shite');
      b.inOrder(wo, kikkake, 1);
      b.inOrder(kikkake, to, 1);
      b.inOrder(to, shite, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, wo, 10);
      b.captureSpan('きっかけ', start, shite);
    },
    // Pattern 4: (noun/verb + の/こと) + がきっかけで
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const ga = b.particle('が', 'ga');
      const de = b.particle('で', 'de');
      b.inOrder(ga, kikkake, 1);
      b.inOrder(kikkake, de, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, ga, 10);
      b.captureSpan('きっかけ', start, de);
    },
    // Pattern 5: (noun/verb + の/こと) + がきっかけになって
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const ga = b.particle('が', 'ga');
      const ni = b.particle('に', 'ni');
      const natte = b.tok({ text: 'なって' }, 'natte');
      b.inOrder(ga, kikkake, 1);
      b.inOrder(kikkake, ni, 1);
      b.inOrder(ni, natte, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, ga, 10);
      b.captureSpan('きっかけ', start, natte);
    },
    // Pattern 6: (noun/verb + の/こと) + がきっかけとなって
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const ga = b.particle('が', 'ga');
      const to = b.particle('と', 'to');
      // Match なって by text or lemma (なる with te-form)
      const natte = b.tok({ textOneOf: ['なって', '成って'] }, 'natte');
      b.inOrder(ga, kikkake, 1);
      b.inOrder(kikkake, to, 1);
      b.inOrder(to, natte, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, ga, 10);
      b.captureSpan('きっかけ', start, natte);
    },
    // Pattern 7: (noun/verb + の/こと) + がきっかけである
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const ga = b.particle('が', 'ga');
      const dearu = b.aux({ lemma: 'だ' }, 'dearu');
      b.inOrder(ga, kikkake, 1);
      b.inOrder(kikkake, dearu, 1);

      const start = b.tok({}, 'start');
      b.inOrder(start, ga, 10);
      b.captureSpan('きっかけ', start, dearu);
    },
    // Pattern 8: きっかけは + (なん/だ/である)
    (b) => {
      const kikkake = b.noun({ lemmaOneOf: ['きっかけ', '切っ掛け', '切っかけ'] }, 'kikkake');
      const wa = b.particle('は', 'wa');
      const end = b.tok({ textOneOf: ['何', 'なん'] }, 'end');
      b.inOrder(kikkake, wa, 1);
      b.inOrder(wa, end, 1);
      b.captureSpan('きっかけ', kikkake, end);
    }
  );
});
