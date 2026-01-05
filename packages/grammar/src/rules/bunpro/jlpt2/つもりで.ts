import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: つもりで (tsumori de) - "with the intention of, acting as if, believing that"
 *
 * An expression that shows doing something with an intention, or pretending/acting
 * as if something were true.
 *
 * Structure:
 * - Verb (dictionary form) + つもりで
 * - Verb (past tense) + つもりで (acting as if/pretending)
 * - Verb (negative) + つもりで
 * - Na-adjective + な + つもりで
 * - Noun + の + つもりで
 *
 * Examples:
 * - 車掌になったつもりで、毎日電車に乗っている。
 *   (I ride the train every day acting like I have become a conductor.)
 * - 何も買わないつもりで新しく出来たショッピングモールへ行ったが、色々買ってしまった。
 *   (I went to the newly built shopping mall without any intention of buying anything, but ended up purchasing various things.)
 * - お土産のつもりで買ったが、食べてしまった。
 *   (I bought it with the intention of giving it as a gift, but I ended up eating it.)
 * - 趣味のつもりで始めた歌でしたが、コンサートを開くまでに成長できました。
 *   (I started singing with the intention of it being a hobby, but it's grown to the point where I have concerts.)
 * - 冗談のつもりで言っただけなのに、相手を傷付けてしまった。
 *   (Despite saying it with the intention of making a joke, I ended up hurting their feelings.)
 *
 * Key discriminators:
 * - Expresses intention, assumption, or pretense when doing something
 * - つもり is a noun meaning "intention"
 * - で indicates "with/by means of"
 * - Different from つもりだ (sentence-final "intend to")
 * - Different from simple instrumental で (with, by)
 *
 * GiNZA parse structure:
 * - Verbs + つもり + で: で is pos=AUX, lemma=だ (copula)
 * - Noun + の + つもり + で: で is pos=ADP, lemma=で, dep=case (case particle)
 * - Na-adj + な + つもり + で: similar to noun pattern
 *
 * The copula "で" (lemma=だ) identifies the grammatical pattern vs instrumental "で" (lemma=で).
 */
export default linguisticRule('つもりで', (r) => {
  r.either(
    // Pattern 1: Verb (any form) + つもりで
    // GiNZA inconsistency: sometimes parses で as copula (lemma=だ), sometimes as case particle (lemma=で)
    // Matches: 行くつもりで, 行ったつもりで, 行かないつもりで, なったつもりで
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const tsumori = b1.noun({ lemma: 'つもり' }, 'tsumori');
      const de = b1.particle('で', 'de');

      b1.inOrder(verb, tsumori, 5);
      b1.inOrder(tsumori, de, 1);

      b1.captureSpan('つもりで', verb, de);
    },

    // Pattern 2: Noun + の + つもりで
    // Matches: お土産のつもりで, 趣味のつもりで, 冗談のつもりで
    (b2) => {
      const noun = b2.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      const no = b2.particle('の', 'no');
      const tsumori = b2.noun({ lemma: 'つもり' }, 'tsumori');
      const de = b2.particle('で', 'de');

      b2.inOrder(noun, no, 1);
      b2.inOrder(no, tsumori, 1);
      b2.inOrder(tsumori, de, 1);

      b2.captureSpan('つもりで', noun, de);
    },

    // Pattern 3: Na-adjective + な + つもりで
    // Matches: 安全なつもりで, 便利なつもりで
    (b3) => {
      const adj = b3.adj({}, 'adj');
      const na = b3.aux({ lemma: 'だ', text: 'な' }, 'na');
      const tsumori = b3.noun({ lemma: 'つもり' }, 'tsumori');
      const de = b3.particle('で', 'de');

      b3.inOrder(adj, na, 1);
      b3.inOrder(na, tsumori, 1);
      b3.inOrder(tsumori, de, 1);

      b3.captureSpan('つもりで', adj, de);
    }
  );
});
