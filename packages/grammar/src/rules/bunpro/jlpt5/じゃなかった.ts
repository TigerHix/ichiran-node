import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('じゃなかった', (r) => {
  // Match the negative past copula じゃなかった (was not)
  // Pattern: noun/na-adj + じゃ/では + なかった
  // Also matches polite variants: じゃありませんでした, ではありませんでした
  // Note: GiNZA uses dep='cop' for nouns, but dep='aux' for na-adjectives
  // Note: "じゃ" has lemma=だ, but "で" in "では" has lemma=で

  r.either(
    // Branch 1: Casual - じゃなかった (single token "じゃ")
    // Structure: noun/na-adj + じゃ + なかっ(fixed) + た(aux)
    (branch) => {
      branch.either(
        // Noun + じゃ + なかった (dep='cop')
        (b1) => {
          const head = b1.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
          const ja = b1.aux({ text: 'じゃ', lemma: 'だ', dep: 'cop' }, 'ja');
          const nai = b1.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.copulaOf(head, ja);
          b1.headChild(ja, nai, 'fixed');
          b1.auxOf(head, ta);
          b1.inOrder(ja, nai, 1);
          b1.inOrder(nai, ta, 1);
          b1.captureSpan('じゃなかった', ja, ta);
        },
        // Na-adjective + じゃ + なかった (dep='aux')
        (b1) => {
          const naAdj = b1.adj({}, 'naAdj');
          const ja = b1.aux({ text: 'じゃ', lemma: 'だ', dep: 'aux' }, 'ja');
          const nai = b1.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.auxOf(naAdj, ja);
          b1.headChild(ja, nai, 'fixed');
          b1.auxOf(naAdj, ta);
          b1.inOrder(ja, nai, 1);
          b1.inOrder(nai, ta, 1);

          // The naAdj must not be an i-adjective
          b1.not((nr) => {
            nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
          });

          b1.captureSpan('じゃなかった', ja, ta);
        }
      );
    },
    // Branch 2: Casual - ではなかった (split "では" into "で" + "は")
    // Structure: noun/na-adj + で(cop/aux) + は(fixed) + なかっ(fixed) + た(aux)
    // Note: For nouns, "で" has lemma=で, dep=cop; for na-adjs, "で" has lemma=だ, dep=aux
    (branch) => {
      branch.either(
        // Noun + では + なかった (dep='cop' for で, lemma=で)
        (b1) => {
          const head = b1.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
          const de = b1.tok({ text: 'で', lemma: 'で', dep: 'cop' }, 'de');
          const wa = b1.tok({ text: 'は', dep: 'fixed' }, 'wa');
          const nai = b1.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.headChild(de, wa, 'fixed');
          b1.headChild(de, nai, 'fixed');
          b1.copulaOf(head, de);
          b1.auxOf(head, ta);
          b1.inOrder(wa, nai, 1);
          b1.inOrder(nai, ta, 1);
          b1.captureSpan('ではなかった', de, ta);
        },
        // Na-adjective + では + なかった (dep='aux' for で, lemma=だ)
        (b1) => {
          const naAdj = b1.adj({}, 'naAdj');
          const de = b1.aux({ text: 'で', lemma: 'だ', dep: 'aux' }, 'de');
          const wa = b1.tok({ text: 'は', dep: 'fixed' }, 'wa');
          const nai = b1.aux({ lemma: 'ない', dep: 'fixed' }, 'nai');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.auxOf(naAdj, de);
          b1.headChild(de, wa, 'fixed');
          b1.headChild(de, nai, 'fixed');
          b1.auxOf(naAdj, ta);
          b1.inOrder(wa, nai, 1);
          b1.inOrder(nai, ta, 1);

          // The naAdj must not be an i-adjective
          b1.not((nr) => {
            nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
          });

          b1.captureSpan('ではなかった', de, ta);
        }
      );
    },
    // Branch 3: Polite - じゃありませんでした (single token "じゃ")
    // Structure: noun/na-adj + じゃ + あり(fixed) + ませ(fixed) + ん(fixed) + でし(cop) + た(aux)
    (branch) => {
      branch.either(
        // Noun + じゃ + ありませんでした (dep='cop')
        (b1) => {
          const head = b1.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
          const ja = b1.aux({ text: 'じゃ', lemma: 'だ', dep: 'cop' }, 'ja');
          const ari = b1.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
          const mase = b1.tok({ lemma: 'ます', dep: 'fixed' }, 'mase');
          const n = b1.tok({ lemma: 'ぬ', dep: 'fixed' }, 'n');
          const deshi = b1.aux({ lemma: 'です', dep: 'cop' }, 'deshi');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.copulaOf(head, ja);
          b1.headChild(ja, ari, 'fixed');
          b1.headChild(ja, mase, 'fixed');
          b1.headChild(ja, n, 'fixed');
          b1.copulaOf(head, deshi);
          b1.auxOf(head, ta);
          b1.inOrder(ja, ari, 1);
          b1.inOrder(ari, mase, 1);
          b1.inOrder(mase, n, 1);
          b1.inOrder(n, deshi, 1);
          b1.inOrder(deshi, ta, 1);
          b1.captureSpan('じゃありませんでした', ja, ta);
        },
        // Na-adjective + じゃ + ありませんでした (dep='aux')
        // Note: "でし" has dep='aux' for na-adjectives (not dep='cop' like for nouns)
        (b1) => {
          const naAdj = b1.adj({}, 'naAdj');
          const ja = b1.aux({ text: 'じゃ', lemma: 'だ', dep: 'aux' }, 'ja');
          const ari = b1.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
          const mase = b1.tok({ lemma: 'ます', dep: 'fixed' }, 'mase');
          const n = b1.tok({ lemma: 'ぬ', dep: 'fixed' }, 'n');
          const deshi = b1.aux({ lemma: 'です', dep: 'aux' }, 'deshi');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.auxOf(naAdj, ja);
          b1.headChild(ja, ari, 'fixed');
          b1.headChild(ja, mase, 'fixed');
          b1.headChild(ja, n, 'fixed');
          b1.auxOf(naAdj, deshi);
          b1.auxOf(naAdj, ta);
          b1.inOrder(ja, ari, 1);
          b1.inOrder(ari, mase, 1);
          b1.inOrder(mase, n, 1);
          b1.inOrder(n, deshi, 1);
          b1.inOrder(deshi, ta, 1);

          // The naAdj must not be an i-adjective
          b1.not((nr) => {
            nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
          });

          b1.captureSpan('じゃありませんでした', ja, ta);
        }
      );
    },
    // Branch 4: Polite - ではありませんでした (split "では" into "で" + "は")
    // Structure: noun/na-adj + で(cop/aux) + は(fixed) + あり(fixed) + ませ(fixed) + ん(fixed) + でし(cop) + た(aux)
    // Note: For nouns, "で" has lemma=で, dep=cop; for na-adjs, "で" has lemma=だ, dep=aux
    (branch) => {
      branch.either(
        // Noun + では + ありませんでした (dep='cop' for で, lemma=で)
        (b1) => {
          const head = b1.tok({ posOneOf: ['NOUN', 'PRON', 'DET', 'NUM'] }, 'head');
          const de = b1.tok({ text: 'で', lemma: 'で', dep: 'cop' }, 'de');
          const wa = b1.tok({ text: 'は', dep: 'fixed' }, 'wa');
          const ari = b1.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
          const mase = b1.tok({ lemma: 'ます', dep: 'fixed' }, 'mase');
          const n = b1.tok({ lemma: 'ぬ', dep: 'fixed' }, 'n');
          const deshi = b1.aux({ lemma: 'です', dep: 'cop' }, 'deshi');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.copulaOf(head, de);
          b1.headChild(de, wa, 'fixed');
          b1.headChild(de, ari, 'fixed');
          b1.headChild(de, mase, 'fixed');
          b1.headChild(de, n, 'fixed');
          b1.copulaOf(head, deshi);
          b1.auxOf(head, ta);
          b1.inOrder(wa, ari, 1);
          b1.inOrder(ari, mase, 1);
          b1.inOrder(mase, n, 1);
          b1.inOrder(n, deshi, 1);
          b1.inOrder(deshi, ta, 1);
          b1.captureSpan('ではありませんでした', de, ta);
        },
        // Na-adjective + では + ありませんでした (dep='aux' for で, lemma=だ)
        // Note: "でし" has dep='aux' for na-adjectives (not dep='cop' like for nouns)
        (b1) => {
          const naAdj = b1.adj({}, 'naAdj');
          const de = b1.aux({ text: 'で', lemma: 'だ', dep: 'aux' }, 'de');
          const wa = b1.tok({ text: 'は', dep: 'fixed' }, 'wa');
          const ari = b1.tok({ lemma: 'ある', dep: 'fixed' }, 'ari');
          const mase = b1.tok({ lemma: 'ます', dep: 'fixed' }, 'mase');
          const n = b1.tok({ lemma: 'ぬ', dep: 'fixed' }, 'n');
          const deshi = b1.aux({ lemma: 'です', dep: 'aux' }, 'deshi');
          const ta = b1.aux({ lemma: 'た', dep: 'aux' }, 'ta');

          b1.auxOf(naAdj, de);
          b1.headChild(de, wa, 'fixed');
          b1.headChild(de, ari, 'fixed');
          b1.headChild(de, mase, 'fixed');
          b1.headChild(de, n, 'fixed');
          b1.auxOf(naAdj, deshi);
          b1.auxOf(naAdj, ta);
          b1.inOrder(wa, ari, 1);
          b1.inOrder(ari, mase, 1);
          b1.inOrder(mase, n, 1);
          b1.inOrder(n, deshi, 1);
          b1.inOrder(deshi, ta, 1);

          // The naAdj must not be an i-adjective
          b1.not((nr) => {
            nr.adj({ conjugationClass: '形容詞' }, 'naAdj');
          });

          b1.captureSpan('ではありませんでした', de, ta);
        }
      );
    }
  );
});
