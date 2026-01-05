import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: だけあって (dake atte) - "as might be expected, worthy of being, just as you'd expect"
 *
 * Expresses that a result is natural or expected based on some quality or circumstance.
 * Similar to だけに but usually used with positive evaluations.
 *
 * Structures:
 * - Verb + だけあって
 * - [い]Adj + だけあって
 * - [な]Adj + な + だけあって
 * - Noun + だけあって
 *
 * Examples:
 * - 彼は10年間日本に住んでいただけあって、日本語を日本人のように話せる。
 *   (As one might expect, he can speak Japanese like a Japanese person because he lived in Japan for 10 years.)
 * - あのホテルは高いだけあって、サービスがとてもいい。
 *   (As one might expect, that hotel has great service because it is expensive.)
 * - 浅草は有名なだけあって、平日でも観光客で賑わっている。
 *   (As might be expected with how popular Asakusa is, it is crowded with tourists even on weekdays.)
 * - 田中先生は習字の先生だけあって、漢字を書くのが上手です。
 *   (As expected, Tanaka-sensei is good at writing kanji because she is a calligraphy teacher.)
 *
 * Key discriminators:
 * - だけ (particle) + あって (te-form of ある)
 * - Must distinguish from:
 *   - だけで (dake de) - "just by/only with" (different grammar)
 *   - だけに (dake ni) - similar meaning but different nuance
 *   - Simple だけ (dake) - "only/just" without あって
 *
 * GiNZA parse structure:
 * - だけ is ADP/PART (助詞-副助詞)
 * - あって is parsed as TWO tokens:
 *   - あっ (lemma=ある, pos=VERB, inflectionForm=連用形-促音便)
 *   - て (lemma=て, pos=SCONJ/AUX)
 * - May be preceded by verbs, adjectives, or nouns
 * - For [な]adjectives and nouns, may have optional の/な before だけ
 */
export default bunproLinguisticRule('だけあって', (r) => {
  r.either(
    // Pattern 1: Verb-te + だけ + あって
    // Most common pattern with verbs in te-form
    // e.g., 住んでいただけあって, 勉強しただけあって, 習っているだけあって
    (b1) => {
      const verbTe = b1.verb({
        inflectionFormOneOf: ['連用形-一般', '連用形-促音便'],
      }, 'verbTe');
      const dake = b1.particle('だけ', 'dake');
      const accu = b1.tok({
        text: 'あっ',
        lemma: 'ある',
        pos: 'VERB',
        inflectionForm: '連用形-促音便',
      }, 'accu');
      const te = b1.tok({
        text: 'て',
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      b1.inOrder(verbTe, dake, 5);
      b1.inOrder(dake, accu, 1);
      b1.inOrder(accu, te, 1);
      b1.captureSpan('だけあって', dake, te);
    },

    // Pattern 2: Noun + (の/な) + だけ + あって
    // Nouns and na-adjectives with optional connective particle
    // e.g., 専門家だけあって, 有名なだけあって, 仲良しなだけあって
    (b2) => {
      const noun = b2.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'],
      }, 'noun');
      const dake = b2.particle('だけ', 'dake');
      const accu = b2.tok({
        text: 'あっ',
        lemma: 'ある',
        pos: 'VERB',
        inflectionForm: '連用形-促音便',
      }, 'accu');
      const te = b2.tok({
        text: 'て',
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      // Optional connective (の/な) between noun and dake
      // Note: 'な' can be ADP or AUX depending on whether it's copula or particle
      b2.optional((ob) => {
        const connective = ob.tok({
          textOneOf: ['の', 'な'],
          posOneOf: ['ADP', 'AUX', 'PART'],
        }, 'connective');
        ob.inOrder(noun, connective, 1);
        ob.inOrder(connective, dake, 1);
      });

      b2.inOrder(noun, dake, 3);
      b2.inOrder(dake, accu, 1);
      b2.inOrder(accu, te, 1);
      b2.captureSpan('だけあって', dake, te);
    },

    // Pattern 3: [い]Adjective + だけ + あって
    // i-adjectives don't need connective particle
    // e.g., 高いだけあって
    (b3) => {
      const adj = b3.adj({}, 'adj');
      const dake = b3.particle('だけ', 'dake');
      const accu = b3.tok({
        text: 'あっ',
        lemma: 'ある',
        pos: 'VERB',
        inflectionForm: '連用形-促音便',
      }, 'accu');
      const te = b3.tok({
        text: 'て',
        posOneOf: ['SCONJ', 'AUX'],
      }, 'te');

      b3.inOrder(adj, dake, 1);
      b3.inOrder(dake, accu, 1);
      b3.inOrder(accu, te, 1);
      b3.captureSpan('だけあって', dake, te);
    }
  );
});
