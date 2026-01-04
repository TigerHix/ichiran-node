import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('あまりに', (r) => {
  // あまりに / あまりにも - "exceedingly, so much that"
  // Adverbial form meaning "excessively" used with adjectives/verbs
  // Also matches あまりの + noun (excessive [noun])
  //
  // Distinct from あまり-ない (JLPT4) which means "not very, not much"
  //
  // GiNZA parses あまり as ADJ (形状詞-一般), not ADV!
  // GiNZA parses に as AUX (lemma=だ, inflectionForm=連用形-ニ), not particle!
  //
  // Patterns:
  // - あまりに + Adjective/Adverb: あまりに早い, あまりに早く, あまりに忙しそうに
  // - あまりに + Na-adj: あまりに非現実的だ, あまりに失礼だ
  // - あまりにも + [adj/verb]: emphatic form (あまりにも幼稚すぎる)
  // - あまりの + Noun: あまりの恐怖 (excessive terror)

  const amari = r.tok({
    textOneOf: ['あまり', 'あんまり', '余り'],
    pos: 'ADJ',
    tag: '形状詞-一般',
  }, 'amari');

  r.either(
    // Pattern 1: あまりに + predicate (any ADJ/VERB/ADV follows)
    // あまりに早く, あまりに忙しそうに, あまりに失礼だ
    (b) => {
      const ni = b.aux({
        text: 'に',
        lemma: 'だ',
        inflectionForm: '連用形-ニ',
        conjugationClass: '助動詞-ダ',
      }, 'ni');
      b.inOrder(amari, ni, 1);
      const pred = b.tok({
        posOneOf: ['ADJ', 'VERB', 'ADV'],
      }, 'pred');
      b.inOrder(ni, pred, 5);
      b.captureSpan('あまりに', amari, pred);
    },

    // Pattern 2: あまりの + Noun (excessive [noun])
    // あまりの恐怖に, あまりの丁寧な言葉遣い
    (b) => {
      const no = b.particle('の', 'no');
      b.inOrder(amari, no, 1);
      const noun = b.tok({
        posOneOf: ['NOUN', 'ADJ'],
      }, 'noun');
      b.inOrder(no, noun, 3);
      b.captureSpan('あまりの', amari, noun);
    },

    // Pattern 3: あまりにも - emphatic form with も
    // あまりにも幼稚すぎる, あまりにも高すぎて
    (b) => {
      const ni = b.aux({
        text: 'に',
        lemma: 'だ',
        inflectionForm: '連用形-ニ',
        conjugationClass: '助動詞-ダ',
      }, 'ni');
      b.inOrder(amari, ni, 1);
      const mo = b.particle('も', 'mo');
      b.inOrder(ni, mo, 1);
      const pred = b.tok({
        posOneOf: ['ADJ', 'VERB', 'ADV'],
      }, 'pred');
      b.inOrder(mo, pred, 5);
      b.captureSpan('あまりにも', amari, pred);
    }
  );
});
