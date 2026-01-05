import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('じゃない', (r) => {
  // Match the casual and polite negative copula (is not)
  // Casual: じゃない, ではない, slang: じゃねえ
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
    // 'は' is ADP, not AUX, so use tok() instead of aux()
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const de = branch.aux({ text: 'で', dep: 'cop' }, 'de');
      const wa = branch.tok({ text: 'は' }, 'wa');
      const nai = branch.aux({ lemma: 'ない' }, 'nai');

      branch.copulaOf(head, de);
      branch.inOrder(de, wa, 1);
      branch.inOrder(wa, nai, 1);
      branch.captureSpan('ではない', de, nai);
    },
    // Branch 4: Na-adjective + で + は + ない (polite ではない)
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const de = branch.aux({ lemma: 'だ', dep: 'aux' }, 'de');
      const wa = branch.tok({ text: 'は' }, 'wa');
      const nai = branch.aux({ lemma: 'ない' }, 'nai');

      branch.auxOf(naAdj, de);
      branch.inOrder(de, wa, 1);
      branch.inOrder(wa, nai, 1);

      // The naAdj must not be an i-adjective
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('ではない', de, nai);
    },
    // Branch 5: Noun + じゃ + あり + ませ + ん (polite じゃありません)
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'cop' }, 'ja');
      const ari = branch.tok({ lemma: 'ある' }, 'ari');
      const mase = branch.aux({ lemma: 'ます' }, 'mase');
      const n = branch.tok({ text: 'ん' }, 'n');

      branch.copulaOf(head, ja);
      branch.inOrder(ja, ari, 1);
      branch.inOrder(ari, mase, 1);
      branch.inOrder(mase, n, 1);
      branch.captureSpan('じゃありません', ja, n);
    },
    // Branch 6: Na-adjective + じゃ + あり + ませ + ん (polite じゃありません)
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'aux' }, 'ja');
      const ari = branch.tok({ lemma: 'ある' }, 'ari');
      const mase = branch.aux({ lemma: 'ます' }, 'mase');
      const n = branch.tok({ text: 'ん' }, 'n');

      branch.auxOf(naAdj, ja);
      branch.inOrder(ja, ari, 1);
      branch.inOrder(ari, mase, 1);
      branch.inOrder(mase, n, 1);

      // The naAdj must not be an i-adjective
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('じゃありません', ja, n);
    },
    // Branch 7: Noun + で + は + あり + ませ + ん (polite ではありません)
    // For nouns: 'で' has lemma 'で' and dep='cop'
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const de = branch.aux({ text: 'で', lemma: 'で', dep: 'cop' }, 'de');
      const wa = branch.tok({ text: 'は' }, 'wa');
      const ari = branch.tok({ lemma: 'ある' }, 'ari');
      const mase = branch.aux({ lemma: 'ます' }, 'mase');
      const n = branch.tok({ text: 'ん' }, 'n');

      branch.copulaOf(head, de);
      branch.inOrder(de, wa, 1);
      branch.inOrder(wa, ari, 1);
      branch.inOrder(ari, mase, 1);
      branch.inOrder(mase, n, 1);
      branch.captureSpan('ではありません', de, n);
    },
    // Branch 8: Na-adjective + で + は + あり + ませ + ん (polite ではありません)
    // For na-adjectives: 'で' has lemma 'だ' and dep='aux'
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const de = branch.aux({ text: 'で', lemma: 'だ', dep: 'aux' }, 'de');
      const wa = branch.tok({ text: 'は' }, 'wa');
      const ari = branch.tok({ lemma: 'ある' }, 'ari');
      const mase = branch.aux({ lemma: 'ます' }, 'mase');
      const n = branch.tok({ text: 'ん' }, 'n');

      branch.auxOf(naAdj, de);
      branch.inOrder(de, wa, 1);
      branch.inOrder(wa, ari, 1);
      branch.inOrder(ari, mase, 1);
      branch.inOrder(mase, n, 1);

      // The naAdj must not be an i-adjective
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('ではありません', de, n);
    },
    // Branch 9: Noun + じゃ + ねえ (slang じゃねえ - casual "ja nee")
    // "ねえ" is the slang pronunciation of "ない" in casual speech
    (branch) => {
      const head = branch.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'cop' }, 'ja');
      const nee = branch.tok({ text: 'ねえ' }, 'nee');

      branch.copulaOf(head, ja);
      branch.inOrder(ja, nee, 1);
      branch.captureSpan('じゃねえ', ja, nee);
    },
    // Branch 10: Na-adjective + じゃ + ねえ (slang じゃねえ - casual "ja nee")
    (branch) => {
      const naAdj = branch.adj({}, 'naAdj');
      const ja = branch.aux({ text: 'じゃ', lemma: 'だ', dep: 'aux' }, 'ja');
      const nee = branch.tok({ text: 'ねえ' }, 'nee');

      branch.auxOf(naAdj, ja);
      branch.inOrder(ja, nee, 1);

      // The naAdj must not be an i-adjective
      branch.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
      });

      branch.captureSpan('じゃねえ', ja, nee);
    }
  );
});
