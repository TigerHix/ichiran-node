import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('はずがない', (r) => {
  // はずがない/はずがありません expresses strong disbelief or impossibility
  // Pattern: plain form + はず + が + ない/ありません
  //
  // GiNZA parses ない as:
  // - pos: ADJ
  // - conjugationClass: 形容詞
  // - inflectionForm: 終止形-一般 (for present), 連用形-促音便 (for past)
  //
  // Forms:
  // - はずがない (casual: verb/adj + はず + が + ない)
  // - なはずがない (な-adj: な + はず + が + ない)
  // - のはずがない (noun: の + はず + が + ない)
  // - はずがありません (polite form)
  // - Past forms: はずがなかった, なはずがなかった, のはずがなかった

  r.either(
    // Pattern 1: Verb + はずがない (casual present)
    // Example: 夏に雪が降るはずがない
    (b) => {
      const verb = b.tok({ pos: 'VERB' }, 'verb');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nai = b.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b.inOrder(verb, hazu);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nai, 1);
      b.captureSpan('はずがない', hazu, nai);
    },

    // Pattern 2: い-adj + はずがない (casual present)
    // Example: 有名人は人気だから、寂しいはずがない
    (b) => {
      const adj = b.adj({}, 'adj');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nai = b.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b.inOrder(adj, hazu);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nai, 1);
      b.captureSpan('はずがない', hazu, nai);
    },

    // Pattern 3: な-adj + なはずがない (casual present)
    // Example: 少年の部屋だから、綺麗なはずがない
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.particle('な', 'na');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nai = b.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b.inOrder(adj, na, 1);
      b.inOrder(na, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nai, 1);
      b.captureSpan('はずがない', hazu, nai);
    },

    // Pattern 4: Noun + のはずがない (casual present)
    // Example: それはあなたのカバンのはずがない
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b.particle('の', 'no');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nai = b.adj({ lemma: 'ない', inflectionForm: '終止形-一般' }, 'nai');

      b.inOrder(noun, no);
      b.inOrder(no, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nai, 1);
      b.captureSpan('はずがない', hazu, nai);
    },

    // Pattern 5: Verb + はずがありません (polite present)
    (b) => {
      const verb = b.tok({ pos: 'VERB' }, 'verb');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const arimasen = b.tok({ lemma: 'ある' }, 'arimasen');
      const polite = b.aux({ lemma: 'ません' }, 'polite');

      b.inOrder(verb, hazu);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, arimasen, 1);
      b.auxOf(arimasen, polite);
      b.captureSpan('はずがない', hazu, polite);
    },

    // Pattern 6: い-adj + はずがありません (polite present)
    (b) => {
      const adj = b.adj({}, 'adj');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const arimasen = b.tok({ lemma: 'ある' }, 'arimasen');
      const polite = b.aux({ lemma: 'ません' }, 'polite');

      b.inOrder(adj, hazu);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, arimasen, 1);
      b.auxOf(arimasen, polite);
      b.captureSpan('はずがない', hazu, polite);
    },

    // Pattern 7: な-adj + なはずがありません (polite present)
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.particle('な', 'na');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const arimasen = b.tok({ lemma: 'ある' }, 'arimasen');
      const polite = b.aux({ lemma: 'ません' }, 'polite');

      b.inOrder(adj, na, 1);
      b.inOrder(na, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, arimasen, 1);
      b.auxOf(arimasen, polite);
      b.captureSpan('はずがない', hazu, polite);
    },

    // Pattern 8: Noun + のはずがありません (polite present)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b.particle('の', 'no');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const arimasen = b.tok({ lemma: 'ある' }, 'arimasen');
      const polite = b.aux({ lemma: 'ません' }, 'polite');

      b.inOrder(noun, no);
      b.inOrder(no, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, arimasen, 1);
      b.auxOf(arimasen, polite);
      b.captureSpan('はずがない', hazu, polite);
    },

    // Pattern 9: Verb + はずがなかった (casual past)
    (b) => {
      const verb = b.tok({ pos: 'VERB' }, 'verb');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nakatta = b.adj({ lemma: 'ない', inflectionForm: '連用形-促音便' }, 'nakatta');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      b.inOrder(verb, hazu);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nakatta, 1);
      b.auxOf(nakatta, ta);
      b.captureSpan('はずがない', hazu, ta);
    },

    // Pattern 10: い-adj + はずがなかった (casual past)
    (b) => {
      const adj = b.adj({}, 'adj');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nakatta = b.adj({ lemma: 'ない', inflectionForm: '連用形-促音便' }, 'nakatta');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      b.inOrder(adj, hazu);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nakatta, 1);
      b.auxOf(nakatta, ta);
      b.captureSpan('はずがない', hazu, ta);
    },

    // Pattern 11: な-adj + なはずがなかった (casual past)
    // Example: 母親が綺麗じゃなかったので、彼女も綺麗なはずがなかった
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.particle('な', 'na');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nakatta = b.adj({ lemma: 'ない', inflectionForm: '連用形-促音便' }, 'nakatta');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      b.inOrder(adj, na, 1);
      b.inOrder(na, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nakatta, 1);
      b.auxOf(nakatta, ta);
      b.captureSpan('はずがない', hazu, ta);
    },

    // Pattern 12: Noun + のはずがなかった (casual past)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b.particle('の', 'no');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const nakatta = b.adj({ lemma: 'ない', inflectionForm: '連用形-促音便' }, 'nakatta');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      b.inOrder(noun, no);
      b.inOrder(no, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, nakatta, 1);
      b.auxOf(nakatta, ta);
      b.captureSpan('はずがない', hazu, ta);
    },

    // Pattern 13: Verb + はずがありませんでした (polite past)
    (b) => {
      const verb = b.tok({ pos: 'VERB' }, 'verb');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const arimasen = b.tok({ lemma: 'ある' }, 'arimasen');
      const polite = b.aux({ lemma: 'ません' }, 'polite');
      const deshita = b.aux({ lemma: 'した' }, 'deshita');

      b.inOrder(verb, hazu);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, arimasen, 1);
      b.auxOf(arimasen, polite);
      b.auxOf(polite, deshita);
      b.captureSpan('はずがない', hazu, deshita);
    },

    // Pattern 14: な-adj + なはずがありませんでした (polite past)
    (b) => {
      const adj = b.adj({}, 'adj');
      const na = b.particle('な', 'na');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const arimasen = b.tok({ lemma: 'ある' }, 'arimasen');
      const polite = b.aux({ lemma: 'ません' }, 'polite');
      const deshita = b.aux({ lemma: 'した' }, 'deshita');

      b.inOrder(adj, na, 1);
      b.inOrder(na, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, arimasen, 1);
      b.auxOf(arimasen, polite);
      b.auxOf(polite, deshita);
      b.captureSpan('はずがない', hazu, deshita);
    },

    // Pattern 15: Noun + のはずがありませんでした (polite past)
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const no = b.particle('の', 'no');
      const hazu = b.noun({ lemma: 'はず' }, 'hazu');
      const ga = b.particle('が', 'ga');
      const arimasen = b.tok({ lemma: 'ある' }, 'arimasen');
      const polite = b.aux({ lemma: 'ません' }, 'polite');
      const deshita = b.aux({ lemma: 'した' }, 'deshita');

      b.inOrder(noun, no);
      b.inOrder(no, hazu, 1);
      b.inOrder(hazu, ga, 1);
      b.inOrder(ga, arimasen, 1);
      b.auxOf(arimasen, polite);
      b.auxOf(polite, deshita);
      b.captureSpan('はずがない', hazu, deshita);
    }
  );
});
