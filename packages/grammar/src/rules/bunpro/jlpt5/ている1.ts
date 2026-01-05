import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ている1', (r) => {
  // Progressive form: Verb[て] + いる (doing something right now)
  // Matches: 食べている (am eating), している (am doing), ねています (is sleeping)
  // Also matches contracted forms like 食べてる, してる
  //
  // GiNZA parses as:
  //   VERB (stem in conjunctive form) + SCONJ (て/で) + AUX/VERB (いる)
  //   Verb's dep varies (root, advcl) depending on sentence position
  //   いる's pos can be AUX or VERB depending on context
  //
  // Examples:
  //   食べている → 食べ [VERB, 連用形-一般] + て [SCONJ, dep=mark] + いる [AUX, dep=aux]
  //   ねています → 寝 [VERB, 連用形-一般] + て [SCONJ, dep=mark] + い [AUX, dep=aux] + ます [AUX, dep=aux]
  //   死んでいる → 死ん [VERB, 連用形-撥音便] + で [SCONJ, dep=mark] + いる [AUX, dep=aux]

  // Match any token before て/で (SCONJ) followed by いる
  // GiNZA tokenization varies widely - some verb stems are VERB, some are AUX, some have other POS
  // We rely on the structural pattern (X + て + いる) and use negative tests to avoid overcapture
  const verb = r.tok({}, 'verb');
  const te = r.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
  const iru = r.tok({ lemma: 'いる', posOneOf: ['AUX', 'VERB'] }, 'iru');

  r.inOrder(verb, te, 1);
  r.inOrder(verb, iru);
  r.inOrder(te, iru, 2);
  r.captureSpan('ている', verb, iru);
});
