import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なくて', (r) => {
  // なくて - Negative te-form conjunction (reasons/causes)
  // Used for connecting clauses with negative meaning, expressing reasons or causes.
  //
  // Patterns:
  // 1. Verb + なくて (negative te-form of verbs)
  //    Examples: 寝れなくて, わからなくて, いけなくて, きていなくて, ださなくて
  // 2. I-adjective + くなくて (negative te-form of i-adjectives)
  //    Examples: あたたかくなくて, よくなくて, 寒くなくて
  // 3. Noun + がなくて (noun + existential negation)
  //    Examples: 時間がなくて, お金がなくて, 仕事がなくて
  //
  // Note: Na-adjective negations (e.g., 静かじゃなくて) and copula negations
  // (e.g., 重病じゃなくて) are handled by the ではなくて-じゃなくて rule (JLPT3).
  //
  // GiNZA parsing patterns:
  // - なく is often parsed as AUX with lemma=ない, but not always dep=fixed
  // - For verb/i-adj negations: text=なく, lemma=ない, varies by dep
  // - For noun negations: text=なく, lemma=ない, dep=aux (attached to noun)
  // - て is always SCONJ with dep=mark
  //
  // Key discriminators from ないで:
  // - なくて: text=なく (conjunctive form)
  // - ないで: text=ない (dictionary form) + で

  r.either(
    // Pattern 1: なく (AUX, lemma=ない) + て
    // This handles verb and i-adj negations like 寝れなくて, あたたかくなくて
    (b1) => {
      const naku = b1.aux({
        text: 'なく',
        lemma: 'ない',
      }, 'naku');

      const te = b1.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      b1.inOrder(naku, te, 1);
      b1.captureSpan('なくて', naku, te);
    },
    // Pattern 2: なく (any POS) + て
    // This handles noun + が + なくて where なく might not be AUX
    // Examples: 時間がなくて, 仕事がなくて, お金がなくて
    (b2) => {
      const naku = b2.tok({
        text: 'なく',
        lemma: 'ない',
      }, 'naku');

      const te = b2.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      b2.inOrder(naku, te, 1);
      b2.captureSpan('なくて', naku, te);
    }
  );
});
