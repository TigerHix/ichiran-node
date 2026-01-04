import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('へいく', (r) => {
  // Match noun + へ + motion verb (go/come toward somewhere)
  // e.g., 学校へいく (go to school), 東京へいきます (head to Tokyo)
  //
  // Focuses on the journey/direction rather than just the destination.
  //
  // Both casual and polite forms:
  // - Casual: noun + へ + いく/くる/来る
  // - Polite: noun + へ + いき/き/き + ます

  r.either(
    // ===== CASUAL FORMS (～へいく/～へくる) =====

    // Pattern 1: Noun/Proper noun + へ + casual motion verb (e.g., 学校へいく, 東京へいく)
    (b) => {
      const place = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'place');
      const he = b.particle('へ', 'he');
      const motionVerb = b.verb({
        lemmaOneOf: ['いく', 'くる', '来る'],
        // NOT in polite form (not inflectionForm=連用形-一般)
      }, 'motionVerb');

      b.caseMarker(place, he);
      b.inOrder(he, motionVerb, 1);
      b.captureSpan('へいく', place, motionVerb);
    },

    // Pattern 2: Pronoun + へ + casual motion verb (e.g., あそこへいく)
    (b) => {
      const place = b.tok({ pos: 'PRON' }, 'place');
      const he = b.particle('へ', 'he');
      const motionVerb = b.verb({
        lemmaOneOf: ['いく', 'くる', '来る'],
      }, 'motionVerb');

      b.caseMarker(place, he);
      b.inOrder(he, motionVerb, 1);
      b.captureSpan('へいく', place, motionVerb);
    },

    // Pattern 3: Noun + の + Noun + へ + casual motion verb (e.g., 友達の家へいく)
    (b) => {
      const place1 = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'place1');
      const no = b.particle('の', 'no');
      const place2 = b.noun({}, 'place2');
      const he = b.particle('へ', 'he');
      const motionVerb = b.verb({
        lemmaOneOf: ['いく', 'くる', '来る'],
      }, 'motionVerb');

      b.inOrder(place1, no, 1);
      b.inOrder(no, place2, 1);
      b.caseMarker(place2, he);
      b.inOrder(he, motionVerb, 1);
      b.captureSpan('へいく', place1, motionVerb);
    },

    // Pattern 4: な-adjective + Noun + へ + casual motion verb (e.g., 綺麗な海へいく)
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.particle('な', 'na');
      const place = b.noun({}, 'place');
      const he = b.particle('へ', 'he');
      const motionVerb = b.verb({
        lemmaOneOf: ['いく', 'くる', '来る'],
      }, 'motionVerb');

      b.inOrder(adj, na, 1);
      b.inOrder(na, place, 1);
      b.caseMarker(place, he);
      b.inOrder(he, motionVerb, 1);
      b.captureSpan('へいく', adj, motionVerb);
    },

    // ===== POLITE FORMS (～へいきます/～へきます) =====

    // Pattern 5: Noun/Proper noun + へ + polite motion verb (e.g., 学校へいきます)
    (b) => {
      const place = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'place');
      const he = b.particle('へ', 'he');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般', // stem before ます
      }, 'motionVerbStem');
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      b.caseMarker(place, he);
      b.inOrder(he, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('へいく', place, masu);
    },

    // Pattern 6: Pronoun + へ + polite motion verb (e.g., あそこへいきます)
    (b) => {
      const place = b.tok({ pos: 'PRON' }, 'place');
      const he = b.particle('へ', 'he');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般',
      }, 'motionVerbStem');
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      b.caseMarker(place, he);
      b.inOrder(he, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('へいく', place, masu);
    },

    // Pattern 7: Noun + の + Noun + へ + polite motion verb (e.g., 友達の家へいきます)
    (b) => {
      const place1 = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'place1');
      const no = b.particle('の', 'no');
      const place2 = b.noun({}, 'place2');
      const he = b.particle('へ', 'he');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般',
      }, 'motionVerbStem');
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      b.inOrder(place1, no, 1);
      b.inOrder(no, place2, 1);
      b.caseMarker(place2, he);
      b.inOrder(he, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('へいく', place1, masu);
    },

    // Pattern 8: な-adjective + Noun + へ + polite motion verb (e.g., 綺麗な海へいきます)
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.particle('な', 'na');
      const place = b.noun({}, 'place');
      const he = b.particle('へ', 'he');
      const motionVerbStem = b.verb({
        lemmaOneOf: ['いく', 'くる'],
        inflectionForm: '連用形-一般',
      }, 'motionVerbStem');
      const masu = b.aux({ lemma: 'ます' }, 'masu');

      b.inOrder(adj, na, 1);
      b.inOrder(na, place, 1);
      b.caseMarker(place, he);
      b.inOrder(he, motionVerbStem, 1);
      b.auxOf(motionVerbStem, masu);

      b.captureSpan('へいく', adj, masu);
    }
  );
});
