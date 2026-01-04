import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('polite-verb-endings', (r) => {
  // Polite verb endings with ます (masu form)
  // Matches verbs in polite form: 食べます, 行きます, 飲みます, etc.
  // Also matches negative and past forms: 食べません, 食べました
  //
  // Structure:
  // - VERB in 連用形-一般 (ren'you form/stem form)
  // - Followed by AUX ます (lemma=ます) with various inflection forms:
  //   - 終止形-一般 for present polite: ～ます
  //   - 未然形-一般 for negative polite: ～ません (ませ + ん)
  //   - 連用形-タ接 for past polite: ～ました
  //
  // Negative cases to exclude:
  // - Casual verb forms (dictionary form, た form, etc.)
  // - Adjective + です (different grammar rule)

  r.either(
    // Pattern 1: Present polite ～ます
    // e.g., 食べます, 行きます, 飲みます
    // GiNZA parses as: VERB (連用形-一般) + ます (終止形-一般, dep=aux)
    (b1) => {
      const verb = b1.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const masu = b1.aux({
        lemma: 'ます',
        inflectionForm: '終止形-一般',
      }, 'masu');
      b1.auxOf(verb, masu);
      b1.captureSpan('polite-verb-endings', verb, masu);
    },

    // Pattern 2: Negative polite ～ません
    // e.g., 食べません, 行きません, 飲みません
    // GiNZA parses as: VERB (連用形-一般) + ませ (ます in 未然形) + ん (ぬ)
    (b2) => {
      const verb = b2.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mase = b2.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'mase');
      b2.auxOf(verb, mase);
      b2.captureSpan('polite-verb-endings', verb, mase);
    },

    // Pattern 3: Past polite ～ました
    // e.g., 食べました, 行きました, 飲みました
    // GiNZA parses as: VERB (連用形-一般) + まし (ます in 連用形-促音便) + た
    (b3) => {
      const verb = b3.verb({
        inflectionForm: '連用形-一般',
      }, 'verb');
      const mashita = b3.aux({
        lemma: 'ます',
        inflectionForm: '連用形-促音便',
      }, 'mashita');
      b3.auxOf(verb, mashita);
      b3.captureSpan('polite-verb-endings', verb, mashita);
    }
  );
});
