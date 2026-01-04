import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('がする', (r) => {
  r.either(
    // Pattern 1: Casual non-past (がする)
    (b) => {
      // Noun must be a sensation word (sound, smell, taste, feeling)
      const sensationNoun = b.noun(
        {
          // Match both simple and compound sensation nouns
          // Simple: 音, 匂い, 味, 感じ
          // Compound: 爆発音, etc. (any noun ending with these)
          lemmaOneOf: [
            '音', '匂い', '味', '感じ',
            // Compound sensation nouns (contain sensation words)
            '爆発音'
          ]
        },
        'sensationNoun'
      );
      const ga = b.particle('が', 'ga');
      const suru = b.verb({ lemma: 'する' }, 'suru');

      // The sensation noun is the subject of する (nsubj dep)
      b.headChild(suru, sensationNoun, 'nsubj');
      // Particle が marks the subject
      b.caseMarker(sensationNoun, ga);
      // ga and suru are close (within 1-2 tokens)
      b.inOrder(ga, suru, 2);
      b.inOrder(sensationNoun, ga, 1);

      b.captureSpan('がする', sensationNoun, suru);
    },
    // Pattern 2: Casual past (がした)
    (b) => {
      const sensationNoun = b.noun(
        {
          lemmaOneOf: [
            '音', '匂い', '味', '感じ',
            '爆発音'
          ]
        },
        'sensationNoun'
      );
      const ga = b.particle('が', 'ga');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      const ta = b.aux({ lemmaOneOf: ['た', 'だ'] }, 'ta');

      b.headChild(suru, sensationNoun, 'nsubj');
      b.caseMarker(sensationNoun, ga);
      b.auxOf(suru, ta);
      b.inOrder(ga, suru, 2);
      b.inOrder(sensationNoun, ga, 1);

      b.captureSpan('がする', sensationNoun, ta);
    },
    // Pattern 3: Polite non-past (がします)
    (b) => {
      const sensationNoun = b.noun(
        {
          lemmaOneOf: [
            '音', '匂い', '味', '感じ',
            '爆発音'
          ]
        },
        'sensationNoun'
      );
      const ga = b.particle('が', 'ga');
      const suru = b.verb({ lemma: 'する' }, 'suru');
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      b.headChild(suru, sensationNoun, 'nsubj');
      b.caseMarker(sensationNoun, ga);
      b.auxOf(suru, masu);
      b.inOrder(ga, suru, 2);
      b.inOrder(sensationNoun, ga, 1);

      b.captureSpan('がする', sensationNoun, masu);
    }
  );
});
