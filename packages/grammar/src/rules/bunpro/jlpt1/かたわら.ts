import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT1: かたわら (katawara) - "while doing X; at the same time as X"
 *
 * A formal phrase emphasizing "in addition to" (A), (B).
 * Indicates doing something secondary alongside a main activity.
 *
 * Patterns:
 * 1. Verb (dictionary/attributive form) + かたわら: 勤めるかたわら, 教えるかたわら
 * 2. Noun + の + かたわら: 本業のかたわら, 仕事のかたわら
 *
 * Examples:
 * - 彼は飲食店を経営しているかたわら、鳶職人としても働いている。
 * - 本業のかたわらバイトなどの副業をしている多数の人々がいる。
 * - 大学に通う傍ら自身の壮絶な幼少期に関する自叙伝を執筆する。
 *
 * Unlike ながら (simultaneous actions), かたわら emphasizes a main activity
 * with a secondary activity done alongside it, not necessarily at the same time.
 *
 * GiNZA parsing notes:
 * - Verb before かたわら is parsed as 連体形-一般 (attributive form)
 * - かたわら is NOUN with lemma=かたわら, dep=compound or dep=nmod
 * - Verbs may be simple (通う), complex (経営している -> いる), or suru-verbs (製造する -> する)
 * - For suru-verbs, match both VERB and AUX (with inflectionForm=連体形-一般)
 */
export default bunproLinguisticRule('かたわら', (r) => {
  r.either(
    // Pattern 1: Verb/AUX (attributive form) + かたわら
    // 通うかたわら, 教えるかたわら, 活動するかたわら, 経営しているかたわら
    // 製造するかたわら (suru-verb), 全うするかたわら (suru-verb)
    // GiNZA parses these in 連体形-一般 (attributive form) since they modify かたわら
    // Match both VERB and AUX to handle suru-verbs (noun+する)
    (b) => {
      const verb = b.tok({
        posOneOf: ['VERB', 'AUX'],
        inflectionFormOneOf: ['連体形-一般', '終止形-一般'],
      }, 'verb');
      const katawara = b.noun({
        lemma: 'かたわら',
      }, 'katawara');
      b.inOrder(verb, katawara, 1);
      b.captureSpan('かたわら', verb, katawara);
    },

    // Pattern 2: Noun + の + かたわら
    // 本業のかたわら, 仕事のかたわら, 子育てのかたわら
    // 勉強のかたわら, ピアノのかたわら
    (b) => {
      const noun = b.noun({}, 'noun');
      const no = b.particle('の', 'no');
      const katawara = b.noun({
        lemma: 'かたわら',
      }, 'katawara');
      b.inOrder(noun, no, 1);
      b.inOrder(no, katawara, 1);
      b.captureSpan('かたわら', noun, katawara);
    }
  );
});
