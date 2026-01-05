import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なくて-conjunction', (r) => {
  // なくて-conjunction: "to not (A), and..." / "not (A), and..."
  // This is a negative conjunctive form showing sequences of events/states where (A) does NOT happen.
  //
  // Different from ないで (without doing) - this focuses on conjunction/reason.
  //
  // GiNZA parses なくて as:
  // - [AUX/ADJ] なく (lemma:ない, inflection:連用形-一般)
  // - [SCONJ] て (lemma:て, dep:mark)
  //
  // Forms:
  // 1. Verb + なくて: あげなくて, 汚れなくて, こなくて
  // 2. I-adj stem + くなくて: 高くなくて, よくなくて, 眠くなくて
  // 3. Noun/Na-adj + ではなくて: 医者ではなくて, 静かではなくて
  // 4. Noun/Na-adj + じゃなくて: 病気じゃなくて, 元気じゃなくて

  // Common pattern: [AUX/ADJ] なく (lemma:ない) + [SCONJ] て
  const naku = r.tok({ lemma: 'ない', textOneOf: ['なく', 'ない'] }, 'naku');
  const te = r.tok({ text: 'て', pos: 'SCONJ', dep: 'mark' }, 'te');

  r.inOrder(naku, te, 1);
  r.captureSpan('なくて-conjunction', naku, te);
});
