import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('verb-て', (r) => {
  // Verb て form (conjunctive form of verbs)
  // Matches verbs in て form used to connect actions or form other grammar patterns
  // て form has multiple uses: "and", "because", "request", etc.
  // This is the basic て form matching - verbs ending in て/いて/って/して/んで/いで
  //
  // GiNZA parses verb て form as:
  //   VERB (stem) + SCONJ (て/で)
  //   VERB has inflectionForm starting with 連用形
  //   SCONJ has text=て/で and dep=mark
  //
  // Examples:
  //   食べて寝る → 食べ [VERB, 連用形-一般, dep=advcl] + て [SCONJ, dep=mark]
  //   読んで返す → 読ん [VERB, 連用形-撥音便, dep=advcl] + で [SCONJ, dep=mark]
  //   登って休む → 登っ [VERB, 連用形-促音便, dep=advcl] + て [SCONJ, dep=mark]
  //
  // Note: When て form appears at the end of a sentence (e.g., requests), the verb has dep=root.
  // When connecting two verbs (sequence), the verb has dep=advcl.
  // We don't constrain dep to allow both uses.

  r.either(
    // Branch for each conjunctive inflection form variant
    // 連用形-一般 (standard conjunctive, e.g., 食べ)
    (b1) => {
      const verb = b1.verb({ inflectionForm: '連用形-一般' }, 'verb');
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      b1.headChild(verb, te, 'mark');
      b1.inOrder(verb, te, 1);
      b1.captureSpan('て-form', verb, te);
    },
    // 連用形-イ音便 (i-sound contraction, e.g., 泳いで)
    (b2) => {
      const verb = b2.verb({ inflectionForm: '連用形-イ音便' }, 'verb');
      const te = b2.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      b2.headChild(verb, te, 'mark');
      b2.inOrder(verb, te, 1);
      b2.captureSpan('て-form', verb, te);
    },
    // 連用形-撥音便 (n-sound contraction, e.g., 読んで, 死んで)
    (b3) => {
      const verb = b3.verb({ inflectionForm: '連用形-撥音便' }, 'verb');
      const te = b3.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      b3.headChild(verb, te, 'mark');
      b3.inOrder(verb, te, 1);
      b3.captureSpan('て-form', verb, te);
    },
    // 連用形-促音便 (geminate contraction, e.g., 待って, 走って)
    (b4) => {
      const verb = b4.verb({ inflectionForm: '連用形-促音便' }, 'verb');
      const te = b4.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      b4.headChild(verb, te, 'mark');
      b4.inOrder(verb, te, 1);
      b4.captureSpan('て-form', verb, te);
    },
    // 連用形-ウ音便 (u-sound contraction, rare)
    (b5) => {
      const verb = b5.verb({ inflectionForm: '連用形-ウ音便' }, 'verb');
      const te = b5.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ' }, 'te');
      b5.headChild(verb, te, 'mark');
      b5.inOrder(verb, te, 1);
      b5.captureSpan('て-form', verb, te);
    }
    // Note: 連用形-ニ and 連用形-融合 are not relevant for verb て form
  );
});
