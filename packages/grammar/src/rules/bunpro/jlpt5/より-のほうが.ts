import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('より-のほうが', (r) => {
  // より～のほうが (yori no hou ga): more ~ than ~
  // Compares and presents the better/more option
  // Patterns: Verb/Adj (A) + より + Verb/Adj (B) + 方（ほう）+ が

  r.either(
    // Pattern 1: Noun + より + Noun + の + 方 + が
    (r1) => {
      const nounA = r1.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'nounA');
      const yori = r1.particle('より', 'yori', { pos: 'ADP', dep: 'case' });
      const nounB = r1.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'nounB');
      const no = r1.particle('の', 'no');
      const hou = r1.noun({ text: '方', lemma: '方' }, 'hou');
      const ga = r1.particle('が', 'ga', { pos: 'PART', dep: 'subj' });

      r1.inOrder(nounA, yori, 1);
      r1.inOrder(yori, nounB, 3);
      r1.inOrder(nounB, no, 1);
      r1.inOrder(no, hou, 1);
      r1.inOrder(hou, ga, 1);

      r1.captureSpan('より-のほうが', nounA, ga);
    },

    // Pattern 2: い-Adjective + より + い-Adjective + 方 + が
    (r2) => {
      const iAdjA = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdjA');
      const yori = r2.particle('より', 'yori', { pos: 'ADP', dep: 'case' });
      const iAdjB = r2.tok({ pos: 'ADJ', conjugationClass: '形容詞' }, 'iAdjB');
      const hou = r2.noun({ text: '方', lemma: '方' }, 'hou');
      const ga = r2.particle('が', 'ga', { pos: 'PART', dep: 'subj' });

      r2.inOrder(iAdjA, yori, 1);
      r2.inOrder(yori, iAdjB, 3);
      r2.inOrder(iAdjB, hou, 2);
      r2.inOrder(hou, ga, 1);

      r2.captureSpan('より-のほうが', iAdjA, ga);
    },

    // Pattern 3: な-Adjective + な + より + な-Adjective + な + 方 + が
    (r3) => {
      const naAdjA = r3.adj({}, 'naAdjA');
      const naA = r3.particle('な', 'na');
      const yori = r3.particle('より', 'yori', { pos: 'ADP', dep: 'case' });
      const naAdjB = r3.adj({}, 'naAdjB');
      const naB = r3.particle('な', 'na');
      const hou = r3.noun({ text: '方', lemma: '方' }, 'hou');
      const ga = r3.particle('が', 'ga', { pos: 'PART', dep: 'subj' });

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
      const yori = r4.particle('より', 'yori', { pos: 'ADP', dep: 'case' });
      const verbB = r4.verb({}, 'verbB');
      const hou = r4.noun({ text: '方', lemma: '方' }, 'hou');
      const ga = r4.particle('が', 'ga', { pos: 'PART', dep: 'subj' });

      r4.inOrder(verbA, yori, 1);
      r4.inOrder(yori, verbB, 3);
      r4.inOrder(verbB, hou, 2);
      r4.inOrder(hou, ga, 1);

      r4.captureSpan('より-のほうが', verbA, ga);
    }
  );
});
