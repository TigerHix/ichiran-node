import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('こそすれ-ない', (r) => {
  // Core pattern: こそ + classical form (すれ/なれ/あれ)
  // こそ is the emphatic particle
  const koso = r.particle('こそ', 'koso');

  // Variant 1: こそすれ (from する)
  // Example: 増えこそすれ, 悪化こそすれ, 払いこそすれ, 感謝こそすれ
  // Note: GiNZA sometimes parses すれ with lemma=する, sometimes with lemma=すれる
  r.either(
    (b) => {
      const sure = b.tok({
        text: 'すれ',
        lemmaOneOf: ['する', 'すれる'],
        pos: 'VERB'
      }, 'sure');
      b.inOrder(koso, sure, 1);
      b.captureSpan('こそすれ', koso, sure);
    },
    // Variant 2: こそなれ (from なる)
    // Example: ためにこそなれ, にこそなれ
    (b) => {
      const nare = b.tok({
        text: 'なれ',
        lemma: 'なれる',
        pos: 'VERB'
      }, 'nare');
      b.inOrder(koso, nare, 1);
      b.captureSpan('こそなれ', koso, nare);
    },
    // Variant 3: こそあれ (from ある)
    // Example: 憎みこそあれ, 能力こそあれ
    (b) => {
      const are = b.tok({
        text: 'あれ',
        lemma: 'ある',
        pos: 'VERB'
      }, 'are');
      b.inOrder(koso, are, 1);
      b.captureSpan('こそあれ', koso, are);
    }
  );
});
