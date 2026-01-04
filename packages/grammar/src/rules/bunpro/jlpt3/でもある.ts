import { linguisticRule } from '../../../engine/lang.js';

export default linguisticRule('でもある', (r) => {
  // でもある (demo aru) - "is also X" / "also is X"
  // Used in sentences like "X is A, and also B" or "While X, it is also Y"
  //
  // This is DIFFERENT from:
  // - でも meaning "even" (e.g., どこでも, なくても) - particle usage
  // - て-form verbs + も (e.g., なくても) - conditional "even if"
  //
  // Patterns:
  // 1. Noun + でもある (copula で + も + ある)
  //    e.g., 電気自動車でもある, 大陸でもある, 多言語話者でもある
  //    GiNZA: で(AUX,dep=cop) + も(ADP,dep=fixed) + ある(VERB,dep=fixed)
  //
  // 2. Na-adjective + でもある (で + も + ある)
  //    e.g., 快適でもある, 危険でもある
  //    GiNZA: で(AUX,lemma=だ,dep=aux,infl=連用形-一般) + も(ADP,dep=fixed) + ある(VERB,dep=fixed)

  r.either(
    // Pattern 1: Noun + で(copula) + も + ある
    // 電気自動車でもある, 大陸でもある, 多言語話者でもある
    // GiNZA: noun(NOUN/PROPN,root) + で(AUX,dep=cop) + も(ADP,dep=fixed) + ある(VERB,dep=fixed)
    //
    // Note: In some cases like "でもあった" (past tense) or complex phrases like
    // "金属の一つでもある", GiNZA may parse:
    // - で with dep=case instead of dep=cop
    // - も with dep=case instead of dep=fixed
    // - ある with dep=compound instead of dep=fixed
    //
    // However, the key discriminator is that で is AUX (copula te-form),
    // not ADP (locative/instrumental particle like 東京で, 鉛筆で)
    (b) => {
      const de = b.aux({
        text: 'で',
        depOneOf: ['cop', 'case'],  // GiNZA inconsistency: sometimes case in past tense
      }, 'de');
      const mo = b.tok({
        text: 'も',
        pos: 'ADP',
        depOneOf: ['fixed', 'case'],  // GiNZA inconsistency: sometimes case in complex phrases
      }, 'mo');
      const aru = b.verb({
        lemma: 'ある',
        depOneOf: ['fixed', 'compound'],  // GiNZA inconsistency: sometimes compound
      }, 'aru');
      b.inOrder(de, mo, 1);
      b.inOrder(mo, aru, 1);
      b.captureSpan('でもある', de, aru);
    },

    // Pattern 2: Na-adjective + で(aux of だ) + も + ある
    // 快適でもある, 危険でもある
    // GiNZA: adj(ADJ) + で(AUX,lemma=だ,dep=aux,infl=連用形-一般) + も(ADP,dep=fixed) + ある(VERB,dep=fixed)
    (b) => {
      const de = b.aux({
        lemma: 'だ',
        text: 'で',
        inflectionForm: '連用形-一般',
        dep: 'aux',
      }, 'de');
      const mo = b.tok({
        text: 'も',
        pos: 'ADP',
        depOneOf: ['fixed', 'case'],
      }, 'mo');
      const aru = b.verb({
        lemma: 'ある',
        depOneOf: ['fixed', 'compound'],
      }, 'aru');
      b.inOrder(de, mo, 1);
      b.inOrder(mo, aru, 1);
      b.captureSpan('でもある', de, aru);
    }
  );
});
