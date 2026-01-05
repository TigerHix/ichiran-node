import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('とは限らない', (r) => {
  // とは限らない (to wa kagiranai) - "not necessarily", "not always the case"
  // Meaning: Expresses that something is not necessarily true or guaranteed
  //
  // Structures:
  // 1. Verb/i-adj + とは + 限らない (casual)
  // 2. Verb/i-adj + とは + 限りません (polite)
  // 3. Na-adj/noun + だ + とは + 限らない (casual)
  // 4. Na-adj/noun + だ + とは + 限りません (polite)
  //
  // Components:
  // - と (quotational particle - marks the preceding phrase as a topic/quote)
  // - は (topic marker particle)
  // - 限ら/限り (forms of 限る verb)
  // - ない/ません (negation)

  // Pattern: と (quotational particle)
  const to = r.particle('と', 'to');

  // Pattern: は (topic marker)
  const wa = r.particle('は', 'wa');

  r.either(
    // Branch 1: Casual form - とは限らない
    // GiNZA parses as: 限ら (VERB, lemma=かぎる, 未然形) + ない (AUX, lemma=ない)
    // Note: In complex sentences, ない may not be a direct aux of かぎら due to clause structure
    (b) => {
      const kagira = b.verb({
        lemma: 'かぎる',
        text: 'かぎら',
        inflectionForm: '未然形-一般',
      }, 'kagira');
      const nai = b.aux({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(to, wa, 1);
      b.inOrder(wa, kagira, 2);
      b.inOrder(kagira, nai, 2); // nai should be within 2 tokens after kagira
      b.captureSpan('とは限らない', to, nai);
    },

    // Branch 2: Polite form - とはかぎりません
    // GiNZA parses as: かぎり (VERB, lemma=かぎる, 連用形) + ませ (AUX, lemma=ます) + ん (AUX, lemma=ぬ)
    // Both ませ and ん are aux of かぎり (both have same head)
    (b) => {
      const kagiri = b.verb({
        lemma: 'かぎる',
        text: 'かぎり',
        inflectionForm: '連用形-一般',
      }, 'kagiri');
      const mas = b.aux({
        lemma: 'ます',
        text: 'ませ',
        inflectionForm: '未然形-一般',
      }, 'mas');
      const n = b.aux({
        lemma: 'ぬ',
        text: 'ん',
      }, 'n');
      b.inOrder(to, wa, 1);
      b.inOrder(wa, kagiri, 2);
      b.auxOf(kagiri, mas);
      b.auxOf(kagiri, n);  // Both aux attach to kagiri
      b.inOrder(mas, n, 1); // Order constraint
      b.captureSpan('とは限らない', to, n);
    }
  );
});
