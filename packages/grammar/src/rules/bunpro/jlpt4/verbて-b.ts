import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('verbて-b', (r) => {
  // Verb て-form used for non-sequence, contrast, or parallel actions
  // Pattern: Verb[て] + Phrase
  // Meaning: "and", "but (contrast)", "while" - non-sequence, parallel/contrasting states
  // e.g., "お姉ちゃんは毎晩勉強して弟は毎晩ゲームをしている。"
  //       (sister studies AND brother plays games - contrast, equal weight)
  //
  // This is the same grammatical structure as verb-て-b (JLPT5 sequential),
  // but with different semantic usage:
  // - JLPT5 verb-て-b: sequential (A AND THEN B)
  // - JLPT4 verbて-b: non-sequence/contrast (A AND/BUT B, equal weight)
  //
  // The distinction is purely semantic/contextual and cannot be reliably
  // distinguished by syntactic structure alone. Both rules will match the
  // same te-form verbs (dep=advcl), which is acceptable since the usage
  // depends on context.

  // Match verb in te-form (連用形 variants)
  // Te-forms can have various dep values:
  // - dep=advcl: standard te-form connecting to main verb
  // - dep=dep: potential forms (e.g., ひけて, おりたためて)
  // - dep=acl: some potential/attribute forms
  r.either(
    // 連用形-一般 (standard conjunctive, e.g., 食べて, 勉強して, ひけて)
    (b1) => {
      const verb = b1.verb({ inflectionForm: '連用形-一般', depOneOf: ['advcl', 'dep', 'acl'] }, 'verb');
      const te = b1.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b1.headChild(verb, te, 'mark');
      b1.inOrder(verb, te, 1);
      b1.captureSpan('verb-te', verb, te);
    },
    // 連用形-イ音便 (i-sound contraction, e.g., 泳いで)
    (b2) => {
      const verb = b2.verb({ inflectionForm: '連用形-イ音便', depOneOf: ['advcl', 'dep', 'acl'] }, 'verb');
      const te = b2.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b2.headChild(verb, te, 'mark');
      b2.inOrder(verb, te, 1);
      b2.captureSpan('verb-te', verb, te);
    },
    // 連用形-撥音便 (n-sound contraction, e.g., 読んで, 死んで)
    (b3) => {
      const verb = b3.verb({ inflectionForm: '連用形-撥音便', depOneOf: ['advcl', 'dep', 'acl'] }, 'verb');
      const te = b3.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b3.headChild(verb, te, 'mark');
      b3.inOrder(verb, te, 1);
      b3.captureSpan('verb-te', verb, te);
    },
    // 連用形-促音便 (geminate contraction, e.g., 待って, 行って)
    (b4) => {
      const verb = b4.verb({ inflectionForm: '連用形-促音便', depOneOf: ['advcl', 'dep', 'acl'] }, 'verb');
      const te = b4.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b4.headChild(verb, te, 'mark');
      b4.inOrder(verb, te, 1);
      b4.captureSpan('verb-te', verb, te);
    },
    // 連用形-ウ音便 (u-sound contraction, rare)
    (b5) => {
      const verb = b5.verb({ inflectionForm: '連用形-ウ音便', depOneOf: ['advcl', 'dep', 'acl'] }, 'verb');
      const te = b5.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      b5.headChild(verb, te, 'mark');
      b5.inOrder(verb, te, 1);
      b5.captureSpan('verb-te', verb, te);
    },
    // Contracted form してて (していて)
    // GiNZA parses this as: こし [ADJ,dep=advcl,lemma=こす] + て [AUX,lemma=てる,dep=aux]
    // This is a GiNZA parsing quirk for colloquial contracted speech
    (b6) => {
      const verb = b6.tok({ posOneOf: ['VERB', 'ADJ'], dep: 'advcl' }, 'verb');
      const auxTe = b6.aux({ lemma: 'てる', dep: 'aux' }, 'aux');
      const te = b6.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');
      b6.headChild(verb, auxTe, 'aux');
      b6.inOrder(verb, auxTe, 1);
      b6.inOrder(auxTe, te, 1);
      b6.captureSpan('verb-te', verb, te);
    }
  );
});
