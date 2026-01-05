import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ことにはならない - "it doesn't mean that/it won't become that"
 *
 * Matches: predicate + [という] + こと + に + は + ならない (negative outcome/result)
 *
 * This expresses that just because something is true, it doesn't necessarily
 * lead to a particular result or conclusion. It's used to deny logical
 * consequences or expectations. Often preceded by からといって or ても.
 *
 * Meaning: "Just because (A) doesn't mean (B)" or "It doesn't necessarily follow that"
 *
 * Structure variants:
 * - Predicate［連体形］+ ことにはならない (casual, present negative, direct)
 * - Predicate［連体形］+ ということにはならない (casual, present negative, with という)
 * - Predicate［連体形］+ ことにはなりません (polite, present negative)
 * - Predicate［連体形］+ ことにはならないです (polite, present negative)
 *
 * The という pattern is more common - it nominalizes the preceding phrase
 * with "to iu" (literally "that which is said"). This adds emphasis and
 * makes the boundary between the two clauses clearer.
 *
 * Contrast with:
 * - ことになる (JLPT3): expresses that something IS decided/will happen
 * - ことになっている (JLPT2): ongoing state of being arranged
 * - ことにはならない: expresses that something is NOT necessarily the case
 *
 * GiNZA parse structure (for "勉強したことにはならない"):
 * - 勉強(NOUN/VERB) --aux--> する(AUX)
 * - する --aux--> た(AUX) [past marker]
 * - た/する --compound--> こと(NOUN)
 * - こと --fixed--> に(ADP)
 * - こと --fixed--> は(ADP) [topic marker]
 * - は --case--> ならない(VERB) [negative of なる]
 *
 * For "できるということにはならない":
 * - できる --mark--> と(ADP) [quote particle]
 * - と --fixed--> いう(VERB)
 * - いう --fixed--> こと(NOUN)
 * - こと --fixed--> に(ADP)
 * - に --fixed--> は(ADP)
 * - は --case--> ならない(VERB)
 *
 * Key insight: This is the negative form of ことになる, with は inserted
 * for emphasis. Most sentences include という for clearer phrase boundary.
 */
export default bunproLinguisticRule('ことにはならない', (r) => {
  r.either(
    // Branch 1: With という - casual present negative (〜ということにはならない)
    (b) => {
      // Quote particle と marks the quoted phrase
      const to = b.particle('と', 'to');

      // Followed by いう (quotative verb, often in dictionary form)
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      b.inOrder(to, iu, 3);

      // Followed by こと (nominalizer)
      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(iu, koto, 3);

      // Followed by に (case marker)
      const ni = b.tok({ lemma: 'に' }, 'ni');
      b.inOrder(koto, ni, 3);

      // Followed by は (topic marker)
      const wa = b.particle('は', 'wa');
      b.inOrder(ni, wa, 3);

      // ならない (negative of なる)
      // Can be parsed as single token or decomposed
      b.either(
        // 1a: Single token ならない
        (b2) => {
          const naranai = b2.verb({
            lemma: 'なる',
            text: 'ならない',
            inflectionForm: '終止形-一般',
          }, 'naranai');
          b2.inOrder(wa, naranai, 3);
          b2.captureSpan('ことにはならない', to, naranai);
        },
        // 1b: Decomposed: なら (lemma=なる, negation stem) + ない (aux)
        (b2) => {
          const nara = b2.verb({
            lemma: 'なる',
            inflectionForm: '未然形-一般',
          }, 'nara');
          b2.inOrder(wa, nara, 3);

          const nai = b2.aux({
            lemma: 'ない',
            inflectionForm: '終止形-一般',
          }, 'nai');
          b2.auxOf(nara, nai);

          b2.captureSpan('ことにはならない', to, nai);
        }
      );
    },
    // Branch 2: With という - polite present negative (〜ということにはなりません)
    (b) => {
      const to = b.particle('と', 'to');
      const iu = b.verb({ lemma: 'いう' }, 'iu');
      b.inOrder(to, iu, 3);

      const koto = b.noun({ lemma: 'こと' }, 'koto');
      b.inOrder(iu, koto, 3);

      const ni = b.tok({ lemma: 'に' }, 'ni');
      b.inOrder(koto, ni, 3);

      const wa = b.particle('は', 'wa');
      b.inOrder(ni, wa, 3);

      // なりません (polite negative)
      // Can be parsed as single token or decomposed
      b.either(
        // 2a: Single token なりません
        (b2) => {
          const narimasen = b2.verb({
            lemma: 'なる',
            text: 'なりません',
            inflectionForm: '終止形-一般',
          }, 'narimasen');
          b2.inOrder(wa, narimasen, 3);
          b2.captureSpan('ことにはならない', to, narimasen);
        },
        // 2b: Decomposed: なり (lemma=なる, stem) + ませ (polite aux) + ん (negative)
        (b2) => {
          const nari = b2.verb({
            lemma: 'なる',
            inflectionForm: '連用形-一般',
          }, 'nari');
          b2.inOrder(wa, nari, 3);

          const mase = b2.aux({
            lemma: 'ます',
            inflectionForm: '連用形-一般',
          }, 'mase');
          b2.auxOf(nari, mase);

          const n = b2.aux({
            lemmaOneOf: ['ぬ', 'ない'],
          }, 'n');
          b2.auxOf(nari, n);

          b2.captureSpan('ことにはならない', to, n);
        }
      );
    },
    // Branch 3: Direct form (no という) - casual present negative (〜ことにはならない)
    (b) => {
      // Preceding predicate (can be verb/adj/noun phrase)
      const pred = b.tok({}, 'pred');

      // Followed by こと (nominalizer) - dep=compound points to pred
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      // Followed by に (case marker, fixed)
      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      // Followed by は (topic marker) - attaches to naranai
      const wa = b.particle('は', 'wa');

      // ならない (negative of なる)
      // Can be parsed as single token or decomposed
      b.either(
        // 1a: Single token ならない
        (b2) => {
          const naranai = b2.verb({
            lemma: 'なる',
            text: 'ならない',
            inflectionForm: '終止形-一般',
          }, 'naranai');
          b2.headChild(wa, naranai, 'case');
          b2.inOrder(ni, wa, 3);
          b2.inOrder(wa, naranai, 3);
          b2.captureSpan('ことにはならない', pred, naranai);
        },
        // 1b: Decomposed: なら (lemma=なる, negation stem) + ない (aux)
        (b2) => {
          const nara = b2.verb({
            lemma: 'なる',
            inflectionForm: '未然形-一般',
          }, 'nara');
          b2.headChild(wa, nara, 'case');
          b2.inOrder(ni, wa, 3);
          b2.inOrder(wa, nara, 3);

          const nai = b2.aux({
            lemma: 'ない',
            inflectionForm: '終止形-一般',
          }, 'nai');
          b2.auxOf(nara, nai);

          b2.captureSpan('ことにはならない', pred, nai);
        }
      );
    },
    // Branch 4: Direct form (no という) - polite present negative (〜ことにはなりません)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const wa = b.particle('は', 'wa');

      // なりません (polite negative)
      // Can be parsed as single token or decomposed
      b.either(
        // 2a: Single token なりません
        (b2) => {
          const narimasen = b2.verb({
            lemma: 'なる',
            text: 'なりません',
            inflectionForm: '終止形-一般',
          }, 'narimasen');
          b2.headChild(wa, narimasen, 'case');
          b2.inOrder(ni, wa, 3);
          b2.inOrder(wa, narimasen, 3);
          b2.captureSpan('ことにはならない', pred, narimasen);
        },
        // 2b: Decomposed: なり (lemma=なる, stem) + ませ (polite aux) + ん (negative)
        (b2) => {
          const nari = b2.verb({
            lemma: 'なる',
            inflectionForm: '連用形-一般',
          }, 'nari');
          b2.headChild(wa, nari, 'case');
          b2.inOrder(ni, wa, 3);
          b2.inOrder(wa, nari, 3);

          const mase = b2.aux({
            lemma: 'ます',
            inflectionForm: '連用形-一般',
          }, 'mase');
          b2.auxOf(nari, mase);

          const n = b2.aux({
            lemmaOneOf: ['ぬ', 'ない'],
          }, 'n');
          b2.auxOf(nari, n);

          b2.captureSpan('ことにはならない', pred, n);
        }
      );
    },
    // Branch 5: Direct form (no という) - polite with です (〜ことにはならないです)
    (b) => {
      const pred = b.tok({}, 'pred');
      const koto = b.noun({ lemma: 'こと', dep: 'compound' }, 'koto');
      b.headChild(pred, koto, 'compound');

      const ni = b.tok({ lemma: 'に', dep: 'fixed' }, 'ni');
      b.inOrder(koto, ni, 1);

      const wa = b.particle('は', 'wa');

      // ならない (adjective form) + です (copula)
      const naranai = b.adj({
        lemma: 'ない',
        text: 'ならない',
        inflectionForm: '終止形-一般',
      }, 'naranai');
      b.headChild(wa, naranai, 'case');
      b.inOrder(ni, wa, 3);
      b.inOrder(wa, naranai, 3);

      const desu = b.aux({
        lemma: 'だ',
        text: 'です',
        inflectionForm: '終止形-一般',
      }, 'desu');
      b.copulaOf(naranai, desu);

      b.captureSpan('ことにはならない', pred, desu);
    }
  );
});
