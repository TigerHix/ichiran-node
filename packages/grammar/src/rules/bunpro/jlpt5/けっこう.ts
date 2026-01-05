import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('けっこう', (r) => {
  // けっこう (kekkou) is an adverb meaning "quite", "fairly", "rather"
  // It can also mean "no thank you" when used with です
  // Matches both hiragana (けっこう) and kanji (結構) forms

  r.either(
    // Branch 1: けっこう as adverb modifying another word (dep='advmod')
    (branch) => {
      const kekkou = branch.adv({ lemmaOneOf: ['けっこう', '結構'], dep: 'advmod' }, 'kekkou');
      branch.capture(kekkou);
    },
    // Branch 2: けっこう as adverb root (rare but possible)
    (branch) => {
      const kekkou = branch.adv({ lemmaOneOf: ['けっこう', '結構'], dep: 'root' }, 'kekkou');
      branch.capture(kekkou);
    },
    // Branch 3: けっこう as adjective root (when used with です for "no thank you")
    // GiNZA parses "結構です" as ADJ+copula (na-adjective usage)
    (branch) => {
      const kekkou = branch.adj({ lemmaOneOf: ['けっこう', '結構'], dep: 'root' }, 'kekkou');
      branch.capture(kekkou);
    }
  );
});
