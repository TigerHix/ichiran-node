import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('より-のほうが', (r) => {
  // より～のほうが (yori no hou ga): more ~ than ~
  // Compares and presents the better/more option
  // Patterns: Verb/Adj (A) + より + Verb/Adj (B) + 方（ほう）+ が

  r.either(
    // Pattern 1: Noun + より + Noun + の + 方 + が
    (r1) => {
      const nounA = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'nounA');
      const yori = r1.tok({ text: 'より', pos: 'ADP', dep: 'case' }, 'yori');
      const nounB = r1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'nounB');
      const no = r1.tok({ text: 'の', pos: 'ADP', dep: 'case' }, 'no');
      const hou = r1.noun({ textOneOf: ['方', 'ほう'], lemmaOneOf: ['方', 'ほう'] }, 'hou');
      const ga = r1.tok({ text: 'が', pos: 'ADP', dep: 'case' }, 'ga');

      r1.inOrder(nounA, yori, 1);
      r1.inOrder(yori, nounB, 3);
      r1.inOrder(nounB, no, 1);
      r1.inOrder(no, hou, 1);
      r1.inOrder(hou, ga, 1);

      r1.captureSpan('より-のほうが', nounA, ga);
    },

    // Pattern 1b: Noun (+ modifiers) + より + Noun (+ modifiers) + の + 方 + が
    // For cases where nouns are modified by adjectives or phrases
    (r1b) => {
      const nounA = r1b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'nounA');
      const yori = r1b.tok({ text: 'より', pos: 'ADP', dep: 'case' }, 'yori');
      const nounB = r1b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'nounB');
      const no = r1b.tok({ text: 'の', pos: 'ADP', dep: 'case' }, 'no');
      const hou = r1b.noun({ textOneOf: ['方', 'ほう'], lemmaOneOf: ['方', 'ほう'] }, 'hou');
      const ga = r1b.tok({ text: 'が', pos: 'ADP', dep: 'case' }, 'ga');

      r1b.inOrder(nounA, yori, 5);  // Allow more distance for modified nouns
      r1b.inOrder(yori, nounB, 10);  // Allow much more distance (e.g., "家に近い公園")
      r1b.inOrder(nounB, no, 3);     // Allow modifiers between noun and の
      r1b.inOrder(no, hou, 1);
      r1b.inOrder(hou, ga, 1);

      r1b.captureSpan('より-のほうが', nounA, ga);
    },

    // Pattern 2: い-Adjective + より + い-Adjective + 方 + が
    (r2) => {
      const iAdjA = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdjA');
      const yori = r2.tok({ text: 'より', pos: 'ADP', dep: 'case' }, 'yori');
      const iAdjB = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdjB');
      const hou = r2.noun({ textOneOf: ['方', 'ほう'], lemmaOneOf: ['方', 'ほう'] }, 'hou');
      const ga = r2.tok({ text: 'が', pos: 'ADP', dep: 'case' }, 'ga');

      r2.inOrder(iAdjA, yori, 1);
      r2.inOrder(yori, iAdjB, 3);
      r2.inOrder(iAdjB, hou, 2);
      r2.inOrder(hou, ga, 1);

      r2.captureSpan('より-のほうが', iAdjA, ga);
    },

    // Pattern 3: な-Adjective + な + より + な-Adjective + な + 方 + が
    (r3) => {
      const naAdjA = r3.adj({}, 'naAdjA');
      const naA = r3.tok({ text: 'な' }, 'na');
      const yori = r3.tok({ text: 'より', pos: 'ADP', dep: 'case' }, 'yori');
      const naAdjB = r3.adj({}, 'naAdjB');
      const naB = r3.tok({ text: 'な' }, 'na');
      const hou = r3.noun({ textOneOf: ['方', 'ほう'], lemmaOneOf: ['方', 'ほう'] }, 'hou');
      const ga = r3.tok({ text: 'が', pos: 'ADP', dep: 'case' }, 'ga');

      r3.inOrder(naAdjA, naA, 1);
      r3.inOrder(naA, yori, 1);
      r3.inOrder(yori, naAdjB, 4);
      r3.inOrder(naAdjB, naB, 1);
      r3.inOrder(naB, hou, 1);
      r3.inOrder(hou, ga, 1);

      r3.captureSpan('より-のほうが', naAdjA, ga);
    },

    // Pattern 4: Verb + より + Verb + 方 + が
    (r4) => {
      const verbA = r4.verb({}, 'verbA');
      const yori = r4.tok({ text: 'より', pos: 'ADP', dep: 'case' }, 'yori');
      const verbB = r4.verb({}, 'verbB');
      const hou = r4.noun({ textOneOf: ['方', 'ほう'], lemmaOneOf: ['方', 'ほう'] }, 'hou');
      const ga = r4.tok({ text: 'が', pos: 'ADP', dep: 'case' }, 'ga');

      r4.inOrder(verbA, yori, 1);
      r4.inOrder(yori, verbB, 10);  // Allow more distance for verb phrases with objects
      r4.inOrder(verbB, hou, 2);
      r4.inOrder(hou, ga, 1);

      r4.captureSpan('より-のほうが', verbA, ga);
    },

    // Pattern 5: Verb + より + Noun + の + 方 + が
    (r5) => {
      const verbA = r5.verb({}, 'verbA');
      const yori = r5.tok({ text: 'より', pos: 'ADP', dep: 'case' }, 'yori');
      const nounB = r5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'nounB');
      const no = r5.tok({ text: 'の', pos: 'ADP', dep: 'case' }, 'no');
      const hou = r5.noun({ textOneOf: ['方', 'ほう'], lemmaOneOf: ['方', 'ほう'] }, 'hou');
      const ga = r5.tok({ text: 'が', pos: 'ADP', dep: 'case' }, 'ga');

      r5.inOrder(verbA, yori, 1);
      r5.inOrder(yori, nounB, 5);
      r5.inOrder(nounB, no, 1);
      r5.inOrder(no, hou, 1);
      r5.inOrder(hou, ga, 1);

      r5.captureSpan('より-のほうが', verbA, ga);
    },

    // Pattern 6: Noun + より + Noun + の + 方 + を (object marker variant)
    (r6) => {
      const nounA = r6.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'nounA');
      const yori = r6.tok({ text: 'より', pos: 'ADP', dep: 'case' }, 'yori');
      const nounB = r6.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'nounB');
      const no = r6.tok({ text: 'の', pos: 'ADP', dep: 'case' }, 'no');
      const hou = r6.noun({ textOneOf: ['方', 'ほう'], lemmaOneOf: ['方', 'ほう'] }, 'hou');
      const wo = r6.tok({ text: 'を', pos: 'ADP', dep: 'case' }, 'wo');

      r6.inOrder(nounA, yori, 1);
      r6.inOrder(yori, nounB, 3);
      r6.inOrder(nounB, no, 1);
      r6.inOrder(no, hou, 1);
      r6.inOrder(hou, wo, 1);

      r6.captureSpan('より-のほうが', nounA, wo);
    }
  );
});
