import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('particle-の', (r) => {
  // Particle + の - Possessive particle construction
  // Combines particles (から, と, へ, で, まで) with の to show ownership/relationship
  // Pattern: Noun(A) + particle + の + Noun(B)
  // Examples: アメリカからのお土産, 彼との関係, サマーキャンプでの生活

  r.either(
    // Pattern 1: からの (from)
    (b) => {
      const noun1 = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun1');
      const kara = b.particle('から', 'kara', { pos: 'ADP', dep: 'case' });
      const no = b.particle('の', 'no', { pos: 'ADP', dep: 'case' });
      const noun2 = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');

      // Order: noun1 + から + の + noun2 (allow some distance for prefixes like お)
      b.inOrder(noun1, kara, 1);
      b.inOrder(kara, no, 1);
      b.inOrder(no, noun2, 3);  // Allow up to 3 tokens for compounds like お土産

      // Dependencies: both particles attach to noun1
      b.headChild(noun1, kara);
      b.headChild(noun1, no);

      b.captureSpan('particle-の', noun1, no);
    },

    // Pattern 2: との (with)
    (b) => {
      const noun1 = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun1');
      const to = b.particle('と', 'to', { pos: 'ADP', dep: 'case' });
      const no = b.particle('の', 'no', { pos: 'ADP', dep: 'case' });
      const noun2 = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');

      b.inOrder(noun1, to, 1);
      b.inOrder(to, no, 1);
      b.inOrder(no, noun2, 3);

      b.headChild(noun1, to);
      b.headChild(noun1, no);

      b.captureSpan('particle-の', noun1, no);
    },

    // Pattern 3: への (to/toward)
    (b) => {
      const noun1 = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun1');
      const e = b.particle('へ', 'e', { pos: 'ADP', dep: 'case' });
      const no = b.particle('の', 'no', { pos: 'ADP', dep: 'case' });
      const noun2 = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');

      b.inOrder(noun1, e, 1);
      b.inOrder(e, no, 1);
      b.inOrder(no, noun2, 3);

      b.headChild(noun1, e);
      b.headChild(noun1, no);

      b.captureSpan('particle-の', noun1, no);
    },

    // Pattern 4: での (at/in/by means of)
    (b) => {
      const noun1 = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun1');
      const de = b.particle('で', 'de', { pos: 'ADP', dep: 'case' });
      const no = b.particle('の', 'no', { pos: 'ADP', dep: 'case' });
      const noun2 = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');

      b.inOrder(noun1, de, 1);
      b.inOrder(de, no, 1);
      b.inOrder(no, noun2, 3);

      b.headChild(noun1, de);
      b.headChild(noun1, no);

      b.captureSpan('particle-の', noun1, no);
    },

    // Pattern 5: までの (until)
    (b) => {
      const noun1 = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun1');
      const made = b.particle('まで', 'made', { pos: 'ADP', dep: 'case' });
      const no = b.particle('の', 'no', { pos: 'ADP', dep: 'case' });
      const noun2 = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun2');

      b.inOrder(noun1, made, 1);
      b.inOrder(made, no, 1);
      b.inOrder(no, noun2, 3);

      b.headChild(noun1, made);
      b.headChild(noun1, no);

      b.captureSpan('particle-の', noun1, no);
    }
  );
});
