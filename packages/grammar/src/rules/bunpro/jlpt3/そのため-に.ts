import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('そのため-に', (r) => {
  // Pattern: そのため(に) - "for that reason", "because of that", "to that end"
  // Connective expression indicating cause. Can appear as そのため or そのために.
  //
  // GiNZA parses this as:
  // - その (DET) + ため (NOUN)
  // - Optional に (ADP) as case particle
  // - Functions as conjunction (dep=obl for conjunction, dep=nmod for possessive)
  //
  // DISCRIMINATOR: Use dep=obl to exclude possessive そのための patterns

  const sono = r.tok({ text: 'その', pos: 'DET' }, 'sono');
  const tame = r.noun({ lemma: 'ため', dep: 'obl' }, 'tame');

  r.headChild(tame, sono, 'det');
  r.inOrder(sono, tame, 1);

  r.either(
    // Pattern 1: そのため (without に)
    // レポートの提出期限が迫っています。そのためしばらくは遊べません。
    (b) => {
      b.captureSpan('そのため', sono, tame);
    },
    // Pattern 2: そのために (with に)
    // 日本語教師になりたいです。そのために頑張って勉強しています。
    (b) => {
      const ni = b.tok({ text: 'に', pos: 'ADP' }, 'ni');
      b.inOrder(tame, ni, 1);
      b.captureSpan('そのために', sono, ni);
    }
  );
});
