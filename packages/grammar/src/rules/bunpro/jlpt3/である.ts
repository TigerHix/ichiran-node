import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('である', (r) => {
  // である - formal written copula (academic, formal writing)
  // Formal version of だ (casual copula) and です (polite copula)
  // Structure: Noun/Na-adj + で (continuative form of だ) + ある (verb "to exist")
  // Meaning: "is", "to be" (formal/written style)
  //
  // Patterns:
  // - Noun + である: 彼はパイロットである (He is a pilot)
  // - Na-adjective + である: 彼はハンサムである (He is handsome)
  // - であった (past tense): 彼はパイロットであった (He was a pilot)
  // - であります (very formal): 彼はパイロットであります
  //
  // GiNZA parsing for である:
  //   NOUN (root, head=-1) <- で (AUX, lemma=だ, dep=cop, head=NOUN) <- ある (VERB, lemma=ある, dep=fixed, head=で)
  //   ADJ (root, head=-1)  <- で (AUX, lemma=だ, dep=cop, head=ADJ)  <- ある (VERB, lemma=ある, dep=fixed, head=で)
  //
  // GiNZA parsing for であった:
  //   NOUN (root, head=-1) <- で (AUX, lemma=だ, dep=cop, head=NOUN) <- あっ (VERB, lemma=ある, dep=fixed, head=で)
  //                                                                    <- た (AUX, lemma=た, dep=aux, head=NOUN)
  //
  // GiNZA parsing for であります:
  //   Similar to である but with ます form

  r.either(
    // Pattern 1: Noun + で + ある (formal copula, present tense, sentence-final)
    // e.g., 彼はパイロットである, 吾輩は猫である
    // Structure: NOUN (root) <- で (cop) <- ある (fixed)
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'NUM'] }, 'noun');
      const de = b1.aux({ text: 'で', lemma: 'だ', dep: 'cop' }, 'de');
      const aru = b1.verb({ text: 'ある', lemma: 'ある', dep: 'fixed' }, 'aru');
      b1.copulaOf(noun, de);
      b1.headChild(de, aru, 'fixed');
      b1.captureSpan('である', de, aru);
    },

    // Pattern 2: Na-adjective + で + ある (formal copula with na-adjective, sentence-final)
    // e.g., 彼はハンサムである, とても面白いドラマである
    // Structure: ADJ (root) <- で (cop) <- ある (fixed)
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const de = b2.aux({ text: 'で', lemma: 'だ', dep: 'cop' }, 'de');
      const aru = b2.verb({ text: 'ある', lemma: 'ある', dep: 'fixed' }, 'aru');
      b2.copulaOf(adj, de);
      b2.headChild(de, aru, 'fixed');

      // The adj must not be an i-adjective
      // Use the same variable name so it checks the already-bound adj
      b2.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'adj');
      });

      b2.captureSpan('である', de, aru);
    },

    // Pattern 3: Attributive use - modifying a noun or nominalized clause
    // e.g., 重要である箇所, 大変であること, であるのである (explanatory)
    // Structure: NOUN/ADJ <- で (aux) <- ある (fixed)
    // When used attributively (before nouns/こと/の), dep changes from 'cop' to 'aux'
    (b3) => {
      const head = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'NUM', 'ADJ'] }, 'head');
      const de = b3.aux({ text: 'で', lemma: 'だ', depOneOf: ['cop', 'aux'] }, 'de');
      const aru = b3.verb({ text: 'ある', lemma: 'ある', dep: 'fixed' }, 'aru');
      b3.headChild(head, de, 'aux');
      b3.headChild(de, aru, 'fixed');

      // The head must not be an i-adjective
      b3.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'head');
      });

      b3.captureSpan('である', de, aru);
    },

    // Pattern 4: Sentence-final explanatory/emphatic use (のである)
    // e.g., ここから悲しいことが起きるのである
    // Structure: ...ので (dep=fixed) + ある (dep=fixed), both point to same head
    (b4) => {
      const de = b4.aux({ text: 'で', lemma: 'だ', dep: 'fixed' }, 'de');
      const aru = b4.verb({ text: 'ある', lemma: 'ある', dep: 'fixed' }, 'aru');
      // Both de and aru should point to the same head
      b4.captureSpan('である', de, aru);
    },

    // Pattern 5: Past tense - であった
    // e.g., 彼はパイロットであった, 我が社の去年の業績は黒字であった
    // Structure: NOUN (root) <- で (cop) <- あっ (fixed) + た (aux, head=root)
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'NUM'] }, 'noun');
      const de = b5.aux({ text: 'で', lemma: 'だ', dep: 'cop' }, 'de');
      const accu = b5.verb({ text: 'あっ', lemma: 'ある', dep: 'fixed' }, 'accu');
      const ta = b5.aux({ text: 'た', lemma: 'た', dep: 'aux' }, 'ta');
      b5.copulaOf(noun, de);
      b5.headChild(de, accu, 'fixed');
      b5.headChild(noun, ta, 'aux');
      b5.captureSpan('である', de, ta);
    },

    // Pattern 6: Very formal - であります
    // e.g., 私達が会えたのは運命であります
    // Structure: NOUN (root) <- で (cop) <- あり (fixed) + ます (aux)
    (b6) => {
      const noun = b6.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'NUM'] }, 'noun');
      const de = b6.aux({ text: 'で', lemma: 'だ', dep: 'cop' }, 'de');
      const ari = b6.verb({ text: 'あり', lemma: 'ある', dep: 'fixed' }, 'ari');
      const masu = b6.aux({ text: 'ます', lemma: 'ます', dep: 'aux' }, 'masu');
      b6.copulaOf(noun, de);
      b6.headChild(de, ari, 'fixed');
      b6.headChild(noun, masu, 'aux');
      b6.captureSpan('である', de, masu);
    },

    // Pattern 7: Na-adjective + であった (past tense with na-adj)
    // e.g., とても面白いドラマであった
    (b7) => {
      const adj = b7.adj({}, 'adj');
      const de = b7.aux({ text: 'で', lemma: 'だ', dep: 'cop' }, 'de');
      const accu = b7.verb({ text: 'あっ', lemma: 'ある', dep: 'fixed' }, 'accu');
      const ta = b7.aux({ text: 'た', lemma: 'た', dep: 'aux' }, 'ta');
      b7.copulaOf(adj, de);
      b7.headChild(de, accu, 'fixed');
      b7.headChild(adj, ta, 'aux');

      // The adj must not be an i-adjective
      b7.not((nr) => {
        nr.adj({ conjugationClass: '形容詞' }, 'adj');
      });

      b7.captureSpan('である', de, ta);
    }
  );
});
