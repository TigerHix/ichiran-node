import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('い-Adjective-くなかった', (r) => {
  // Match i-adjectives in negative past form (～くなかった)

  // Simplified approach: just match the three key components in sequence
  r.either(
    // Casual form: (い-adj ku-form) + ない + た
    (b) => {
      const adj = b.tok({
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adj');

      const nakatta = b.tok({
        lemma: 'ない',
        inflectionForm: '連用形-促音便',
      }, 'nakatta');

      const ta = b.aux({
        lemma: 'た',
        inflectionForm: '終止形-一般',
      }, 'ta');

      b.inOrder(adj, nakatta, 5);
      b.inOrder(nakatta, ta, 2);

      b.captureSpan('い-Adjective-くなかった', adj, ta);
    },
    // Polite form: (い-adj ku-form) + ない + た + です
    (b) => {
      const adj = b.tok({
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adj');

      const nakatta = b.tok({
        lemma: 'ない',
        inflectionForm: '連用形-促音便',
      }, 'nakatta');

      const ta = b.aux({
        lemma: 'た',
        inflectionForm: '終止形-一般',
      }, 'ta');

      const desu = b.aux({
        lemma: 'です',
        inflectionForm: '終止形-一般',
      }, 'desu');

      b.inOrder(adj, nakatta, 5);
      b.inOrder(nakatta, ta, 2);
      b.inOrder(ta, desu, 2);

      b.captureSpan('い-Adjective-くなかった', adj, desu);
    },
    // Very polite form: (い-adj ku-form) + あります + ません + ...でした
    (b) => {
      const adj = b.tok({
        inflectionForm: '連用形-一般',
        conjugationClass: '形容詞',
      }, 'adj');

      const aru = b.tok({
        pos: 'VERB',
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'aru');

      const masen = b.aux({
        lemma: 'ます',
        inflectionForm: '未然形-一般',
      }, 'masen');

      b.inOrder(adj, aru, 5);
      b.auxOf(aru, masen);

      b.captureSpan('い-Adjective-くなかった', adj, masen);
    }
  );
});
