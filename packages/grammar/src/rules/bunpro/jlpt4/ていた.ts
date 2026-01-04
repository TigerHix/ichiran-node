import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('ていた', (r) => {
  // ていた = verb te-form + いた/いました (past of いる)
  // Matches: 待っていた, 食べていた, 見ていた, していた, etc.
  // Meaning: "was doing", past continuous aspect
  //
  // GiNZA parse structure:
  //   待っていた: 待っ(VERB,root) + て(SCONJ,mark,head→0) + い(VERB,fixed,head→1) + た(AUX,aux,head→0)
  //   していた: し(VERB,root) + て(SCONJ,mark,head→0) + い(VERB,fixed,head→1) + た(AUX,aux,head→0)
  //   勉強していた: 勉強(VERB) + し(AUX,head→勉強) + て(SCONJ,head→勉強) + い(VERB,fixed,head→て) + た(AUX,head→勉強)
  //   していました: い(VERB) + まし(AUX,lemma=ます) + た(AUX,lemma=た)
  //
  // Key discriminators:
  // - い is VERB with lemma=いる, text=い, dep=fixed, head points to て
  // - い must be in 連用形-一般 (continuative form), NOT 未然形-一般 (negative form like いなかった)
  // - た is AUX with lemma=た, dep=aux, head points to main verb
  // - て/で is SCONJ with dep=mark between verb and い

  r.either(
    // Branch 1: Verb-て + い + た (casual past continuous)
    // Example: 待っていた, していた, 座っていた, なっていた, 勉強していた
    (b) => {
      const te = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b.tok({
        lemma: 'いる',
        text: 'い',
        pos: 'VERB',
        dep: 'fixed',
        inflectionForm: '連用形-一般'  // Exclude negative forms (いなかった)
      }, 'iru');
      const ta = b.aux({ lemma: 'た' }, 'ta');

      // Structural constraints
      b.inOrder(te, iru, 1);
      b.headChild(te, iru, 'fixed');  // い's head points to て

      // te must have dep=mark pointing to some VERB/AUX (the main verb)
      // We don't constrain verbTe directly - we just require that te has the right structure
      // and ta has dep=aux pointing to the same verb as te

      // Capture full span from te to ta (includes ていた)
      b.captureSpan('ていた', te, ta);
    },

    // Branch 2: Verb-て + い + まし + た (polite past continuous)
    // Example: していました, なっていました, かけていました
    (b) => {
      const te = b.tok({ textOneOf: ['て', 'で'], pos: 'SCONJ', dep: 'mark' }, 'te');
      const iru = b.tok({
        lemma: 'いる',
        text: 'い',
        pos: 'VERB',
        dep: 'fixed',
        inflectionForm: '連用形-一般'  // Exclude negative forms
      }, 'iru');
      const mashita = b.aux({ lemma: 'た' }, 'ta');

      // Require ます in the chain (lemma=ます)
      const masu = b.aux({ lemma: 'ます' }, 'masu');
      b.headChild(iru, masu, 'aux');
      b.headChild(masu, mashita, 'aux');

      // Other constraints
      b.inOrder(te, iru, 1);
      b.headChild(te, iru, 'fixed');

      // Capture full span from te to ta (includes ていました)
      b.captureSpan('ていました', te, mashita);
    }
  );
});
