import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('がある', (r) => {
  r.either(
    // Pattern 1: Simple affirmative (ある)
    (b) => {
      const ga = b.particle('が', 'ga');
      const aru = b.tok({ lemma: 'ある', pos: 'VERB' }, 'aru');
      b.inOrder(ga, aru, 1);
      b.captureSpan('がある', ga, aru);
    },
    // Pattern 2: Polite affirmative (あります)
    (b) => {
      const ga = b.particle('が', 'ga');
      const aru = b.tok({ lemma: 'ある', pos: 'VERB', inflectionForm: '連用形-一般' }, 'aru');
      const masu = b.tok({ lemma: 'ます', pos: 'AUX' }, 'masu');
      b.auxOf(aru, masu);
      b.inOrder(ga, aru, 1);
      b.captureSpan('がある', ga, masu);
    },
    // Pattern 3: Simple negative (ない) - irregular form, GiNZA parses as ADJ
    (b) => {
      const ga = b.particle('が', 'ga');
      const nai = b.tok({ lemma: 'ない', pos: 'ADJ' }, 'nai');
      b.inOrder(ga, nai, 1);
      b.captureSpan('がある', ga, nai);
    },
    // Pattern 4: Polite negative (ありません)
    (b) => {
      const ga = b.particle('が', 'ga');
      const aru = b.tok({ lemma: 'ある', pos: 'VERB', inflectionForm: '未然形-一般' }, 'aru');
      const masen = b.tok({ lemma: 'ません', pos: 'AUX' }, 'masen');
      b.auxOf(aru, masen);
      b.inOrder(ga, aru, 1);
      b.captureSpan('がある', ga, masen);
    }
  );
});
