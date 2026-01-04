import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('じゃない', (r) => {
  // Match the casual and polite negative copula (is not)
  // Casual: じゃない, ではない
  // Polite: じゃありません, ではありません
  // じゃ is a contraction of では
  // Note: GiNZA uses dep='cop' for nouns, but dep='aux' for na-adjectives

  r.either(
    // Branch 1: Noun + じゃ + ない (dep='cop')
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'cop' }, 'ja');
      const nai = branch.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');

      branch.copulaOf(head, ja);
      branch.headChild(ja, nai, 'fixed');
      branch.captureSpan('じゃない', ja, nai);
    },
    // Branch 2: Na-adjective + じゃ + ない (dep='aux')
    // Must NOT be an i-adjective
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'aux' }, 'ja');
      const nai = branch.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');

      branch.auxOf(naAdj, ja);
      branch.headChild(ja, nai, 'fixed');

      // The naAdj must not be an i-adjective
      // Use the same variable name so it checks the already-bound naAdj
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('じゃない', ja, nai);
    },
    // Branch 3: Noun + で + は + ない (polite ではない)
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const de = branch.aux({ text: 'で', dep: 'cop' }, 'de');
      const wa = branch.aux({ text: 'は', dep: 'fixed' }, 'wa');
      const nai = branch.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');

      branch.copulaOf(head, de);
      branch.headChild(de, wa, 'fixed');
      branch.headChild(de, nai, 'fixed');
      branch.inOrder(de, wa, nai, 2);
      branch.captureSpan('ではない', de, nai);
    },
    // Branch 4: Na-adjective + で + は + ない (polite ではない)
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const de = branch.aux({ lemma: 'だ', dep: 'aux' }, 'de');
      const wa = branch.aux({ text: 'は', dep: 'fixed' }, 'wa');
      const nai = branch.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');

      branch.auxOf(naAdj, de);
      branch.headChild(de, wa, 'fixed');
      branch.headChild(de, nai, 'fixed');
      branch.inOrder(de, wa, nai, 2);

      // The naAdj must not be an i-adjective
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('ではない', de, nai);
    },
    // Branch 5: Noun + じゃ + あり + ませ + ん (polite じゃありません)
    // All subsequent auxiliaries point to 'じゃ' with dep='fixed'
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'cop' }, 'ja');
      const ari = branch.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
      const mase = branch.aux({ lemma: 'ます', dep: 'fixed' }, 'mase');
      const n = branch.aux({ text: 'ん', dep: 'fixed' }, 'n');

      branch.copulaOf(head, ja);
      branch.headChild(ja, ari, 'fixed');
      branch.headChild(ja, mase, 'fixed');
      branch.headChild(ja, n, 'fixed');
      branch.inOrder(ja, ari, mase, n, 4);
      branch.captureSpan('じゃありません', ja, n);
    },
    // Branch 6: Na-adjective + じゃ + あり + ませ + ん (polite じゃありません)
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'aux' }, 'ja');
      const ari = branch.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
      const mase = branch.aux({ lemma: 'ます', dep: 'fixed' }, 'mase');
      const n = branch.aux({ text: 'ん', dep: 'fixed' }, 'n');

      branch.auxOf(naAdj, ja);
      branch.headChild(ja, ari, 'fixed');
      branch.headChild(ja, mase, 'fixed');
      branch.headChild(ja, n, 'fixed');
      branch.inOrder(ja, ari, mase, n, 4);

      // The naAdj must not be an i-adjective
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('じゃありません', ja, n);
    },
    // Branch 7: Noun + で + は + あり + ませ + ん (polite ではありません)
    // For nouns: 'で' has lemma 'で' and dep='cop', subsequent tokens point to 'で'
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const de = branch.aux({ text: 'で', lemma: 'で', dep: 'cop' }, 'de');
      const wa = branch.aux({ text: 'は', dep: 'fixed' }, 'wa');
      const ari = branch.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
      const mase = branch.aux({ lemma: 'ます', dep: 'fixed' }, 'mase');
      const n = branch.aux({ text: 'ん', dep: 'fixed' }, 'n');

      branch.copulaOf(head, de);
      branch.headChild(de, wa, 'fixed');
      branch.headChild(de, ari, 'fixed');
      branch.headChild(de, mase, 'fixed');
      branch.headChild(de, n, 'fixed');
      branch.inOrder(de, wa, ari, mase, n, 5);
      branch.captureSpan('ではありません', de, n);
    },
    // Branch 8: Na-adjective + で + は + あり + ませ + ん (polite ではありません)
    // For na-adjectives: 'で' has lemma 'だ' and dep='aux'
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const de = branch.aux({ text: 'で', lemma: 'だ', dep: 'aux' }, 'de');
      const wa = branch.aux({ text: 'は', dep: 'fixed' }, 'wa');
      const ari = branch.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
      const mase = branch.aux({ lemma: 'ます', dep: 'fixed' }, 'mase');
      const n = branch.aux({ text: 'ん', dep: 'fixed' }, 'n');

      branch.auxOf(naAdj, de);
      branch.headChild(de, wa, 'fixed');
      branch.headChild(de, ari, 'fixed');
      branch.headChild(de, mase, 'fixed');
      branch.headChild(de, n, 'fixed');
      branch.inOrder(de, wa, ari, mase, n, 5);

      // The naAdj must not be an i-adjective
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('ではありません', de, n);
    }
  );
});
