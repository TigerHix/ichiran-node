import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('の-noun-ommission', (r) => {
  // の as noun omission nominalizer
  // Pattern: [NOUN/PROPN/PRON] の (end of sentence or before copula/particle)
  // The noun after の is omitted because it was previously mentioned or is clear from context
  //
  // This is DISTINCT from:
  // - の (possessive): Owner + の + NOUN (explicit second noun)
  // - のは (nominalizer): VERB + の + は/が/も
  //
  // Key discriminators:
  // 1. Noun omission requires NOUN/PRONOUN + の (not VERB + の)
  // 2. For copula patterns: either の+copula directly OR の+は+copula
  // 3. Possessive patterns (owner + の + noun + copula) should NOT match

  r.either(
    // Pattern 1: NOUN/PRONOUN + の + copula/aux (allows clause between owner and の)
    // e.g., たけしさんのです, 私のじゃない, ヨシのだ, あなたが乗っているのです
    // Excludes: 私の本です (noun 本 between の and です)
    (b1) => {
      const owner = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'owner');
      const no = b1.particle('の', 'no');
      const copula = b1.tok({ posOneOf: ['AUX', 'VERB'] }, 'copula');

      b1.inOrder(owner, no, 10); // owner can be up to 10 tokens before (for names with clauses)
      b1.inOrder(no, copula, 1); // copula MUST be immediately after の
      b1.capture(no);
    },
    // Pattern 2: NOUN/PRONOUN + の + は + copula/ADJ (topic marker then copula)
    // e.g., 木綿のスカーフは私のです, その帽子は娘のです, 日本のは美味しい
    // Note: The の+は makes the phrase the topic (noun omission pattern)
    (b2) => {
      const owner = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'owner');
      const no = b2.particle('の', 'no');
      const wa = b2.particle('は', 'wa');
      const copulaOrAdj = b2.tok({ posOneOf: ['AUX', 'VERB', 'ADJ'] }, 'copulaOrAdj');

      b2.inOrder(owner, no, 10);
      b2.inOrder(no, wa, 1); // は MUST be immediately after の
      b2.inOrder(wa, copulaOrAdj, 10);
      b2.capture(no);
    },
    // Pattern 3: NOUN/PRONOUN + の + particle (で, etc.) immediately
    // e.g., 友達ので行った (I went on [a bicycle] of a friend)
    (b3) => {
      const owner = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'owner');
      const no = b3.particle('の', 'no');
      const particle = b3.tok({ pos: 'ADP', dep: 'case' }, 'particle');

      b3.inOrder(owner, no, 10);
      b3.inOrder(no, particle, 1); // particle MUST be immediately after の
      b3.capture(no);
    },
    // Pattern 4: NOUN/PRONOUN + の + が/も + copula (nominalizer + subject/also marker)
    // e.g., あなたが乗っているのですか, あそこにある服は私のだけど
    (b4) => {
      const owner = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'owner');
      const no = b4.particle('の', 'no');
      const gaOrMo = b4.tok({ textOneOf: ['が', 'も'], pos: 'ADP' }, 'gaOrMo');
      const copula = b4.tok({ posOneOf: ['AUX', 'VERB'] }, 'copula');

      b4.inOrder(owner, no, 10);
      b4.inOrder(no, gaOrMo, 1); // が/も MUST be immediately after の
      b4.inOrder(gaOrMo, copula, 10);
      b4.capture(no);
    },
    // Pattern 5: NOUN/PRONOUN + の at sentence end (with nothing or just punctuation after)
    // e.g., 誰の？, あなたの？, ヨシの
    (b5) => {
      const owner = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'owner');
      const no = b5.particle('の', 'no');
      const punct = b5.tok({ pos: 'PUNCT' }, 'punct');

      b5.inOrder(owner, no, 10);
      b5.inOrder(no, punct, 2); // punctuation must be within 1-2 tokens
      b5.capture(no);
    }
  );
});
