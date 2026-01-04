import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('できれば-できたら', (r) => {
  // できれば/できたら (if possible) - conditional expressions using できる
  // Both mean "if possible" and are used at the beginning of sentences
  // to make polite requests or express desires

  r.either(
    // Pattern 1: できれば (dekireba) - conditional form with ば
    // e.g., できれば車で行きたい, できればあの会社で働きたい
    // GiNZA: できれ (VERB, lemma=できる, infl=仮定形-一般) + ば (SCONJ, dep=mark)
    (b) => {
      const deki = b.verb({
        lemma: 'できる',
        inflectionForm: '仮定形-一般',
      }, 'deki');
      const ba = b.tok({
        text: 'ば',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'ba');
      b.inOrder(deki, ba, 1);
      b.captureSpan('できれば', deki, ba);
    },
    // Pattern 2: できたら (dekitara) - conditional form with たら
    // e.g., できたら薬局に行って, それができたら嬉しい
    // GiNZA: でき (VERB, lemma=できる, infl=連用形-一般) + たら (AUX, lemma=た, infl=仮定形-一般)
    (b) => {
      const deki = b.verb({
        lemma: 'できる',
        inflectionForm: '連用形-一般',
      }, 'deki');
      const tara = b.aux({
        lemma: 'た',
        inflectionForm: '仮定形-一般',
      }, 'tara');
      b.auxOf(deki, tara);
      b.captureSpan('できたら', deki, tara);
    }
  );
});
