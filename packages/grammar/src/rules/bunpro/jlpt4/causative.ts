import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('causative', (r) => {
  // Causative form: Verb + せる/させる (make/let someone do)
  //
  // Regular verbs: verb stem + せる/させる
  //   - 食べる → 食べさせる (iru/eru verb)
  //   - 行く → 行かせる (godan verb)
  //   - 歌う → 歌わせる (godan verb with -u)
  //   - 読む → 読ませる (godan verb with -mu)
  //   - 泣く → 泣かせる (godan verb with -ku)
  //   - 話す → 話させる (godan verb with -su)
  //   - 死ぬ → 死なせる (godan verb with -nu)
  //   - 飛ぶ → 飛ばせる (godan verb with -bu)
  //   - 休む → 休ませる (godan verb with -mu)
  //   - 泳ぐ → 泳がせる (godan verb with -gu)
  //
  // Irregular verbs:
  //   - する → させる
  //   - くる → こさせる / 来させる
  //
  // GiNZA parsing patterns:
  //   - Regular verbs: VERB (inflectionForm: 未然形-一般) + AUX (lemma: せる/させる, dep: aux)
  //   - Suru-verbs: VERB (lemma: する, inflectionForm: 未然形-サ) + AUX (lemma: せる/させる, dep: aux)
  //   - Kuru: VERB (lemma: 来る) + AUX (lemma: させる, dep: aux)
  //   - Note: くる irregular may also parse as こ (VERB, lemma: 来る) + させる (AUX)
  //
  // Causative-passive exclusion:
  //   Causative-passive has せる/させる in 未然形-一般 (irrealis) form before passive aux
  //   We only match せる/させる in final forms (終止形, 連用形, 連体形)

  r.either(
    // Branch 1: Regular verbs with せる (seru)
    // Verb in irrealis form (未然形-一般 or 未然形-サ) + せる auxiliary
    // Only match when せる is in final form, not irrealis form (excludes causative-passive)
    (b) => {
      const seru = b.aux({
        lemma: 'せる',
        inflectionFormOneOf: ['終止形-一般', '連用形-一般', '連体形-一般'],
      }, 'seru');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionFormOneOf: ['未然形-一般', '未然形-サ'],
      }, 'verb');

      b.auxOf(verb, seru);
      b.inOrder(verb, seru, 5);
      b.captureSpan('causative', verb, seru);
    },

    // Branch 2: Regular verbs with させる (saseru)
    // Verb in irrealis form (未然形-一般) or auxiliary form + させる auxiliary
    // Only match when させる is in final form, not irrealis form (excludes causative-passive)
    (b) => {
      const saseru = b.aux({
        lemma: 'させる',
        inflectionFormOneOf: ['終止形-一般', '連用形-一般', '連体形-一般'],
      }, 'saseru');

      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionFormOneOf: ['未然形-一般', '連用形-一般', '未然形-サ'],
      }, 'verb');

      b.auxOf(verb, saseru);
      b.inOrder(verb, saseru, 5);
      b.captureSpan('causative', verb, saseru);
    }
  );
});
