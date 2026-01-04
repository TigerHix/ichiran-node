import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('せいで', (r) => {
  // Pattern: noun/verb/adj + せいで (because of / due to - negative consequence)
  // せいで expresses blame or assigns responsibility for a negative result
  //
  // GiNZA parses せいで as:
  // - せい (NOUN) with dep=obl (when followed by で)
  // - で as ADP with lemma=で, dep=case, head pointing to せい
  //
  // Also matches せい + copula (で omitted):
  // - せい (NOUN) with dep=root
  // - です/でしょう as AUX with dep=cop, head pointing to せい
  //
  // Also matches せい + で + も + ない pattern:
  // - せい (NOUN) with dep=advcl
  // - で as AUX with dep=cop, lemma=で
  //
  // This matches patterns like:
  // - 湿気のせいでカビがひどい (noun + の + せいで)
  // - 外が寒いせいで風邪を引いた (i-adj + せいで)
  // - 気分が悪くなったせいで、仕事を休んでしまった (verb + た + せいで)
  // - 髪型のせいですよ (noun + の + せい + です)
  // - 年のせいでしょう (noun + の + せい + でしょう)
  // - 誰のせいでもない (noun + の + せい + で + も + ない)

  r.either(
    // Pattern 1: せいで (full form)
    // 湿気のせいでカビがひどい。
    (b) => {
      const sei = b.noun({ lemma: 'せい', dep: 'obl' }, 'sei');
      const de = b.tok({ pos: 'ADP', lemma: 'で', dep: 'case' }, 'de');
      b.caseMarker(sei, de);
      b.captureSpan('せいで', sei, de);
    },
    // Pattern 2: せいです / せいでしょう (copula form)
    // 髪型のせいですよ。
    // 年のせいでしょう。
    (b) => {
      const sei = b.noun({ lemma: 'せい', dep: 'root' }, 'sei');
      const copula = b.aux({ dep: 'cop' }, 'copula');
      b.copulaOf(sei, copula);
      b.captureSpan('せい', sei, copula);
    },
    // Pattern 3: せいでも (advcl form - used in negative constructions)
    // 誰のせいでもないと思います。
    (b) => {
      const sei = b.noun({ lemma: 'せい', dep: 'advcl' }, 'sei');
      const de = b.aux({ lemma: 'で', dep: 'cop' }, 'de');
      b.copulaOf(sei, de);
      b.captureSpan('せいで', sei, de);
    }
  );
});
