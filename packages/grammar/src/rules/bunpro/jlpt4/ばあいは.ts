import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('ばあいは', (r) => {
  // 場合は (in the case of / in the event of)
  // Pattern analysis:
  // - Verb + 場合(は): 行く場合は, 遊んでいる場合は
  // - い-adjective + 場合(は): 汚い場合は, ひどい場合は
  // - な-adjective + な場合(は): 危険な場合は, 大変な場合は
  // - Noun + の場合(は): 私の場合は, 私の犬の場合は
  //
  // 場合 can be either NOUN or PROPN (proper noun in some parses)
  // The particle は is optional (cases without は exist)
  //
  // Key discriminators:
  // - 場合 lemma: "ばあい" (hiragana) or "場合" (kanji) - GiNZA uses hiragana lemma
  // - Text can be either "ばあい" or "場合" (both normalized to 場合)
  // - Must follow a modifying element (verb, adjective, or noun+の)
  // - The は topic particle is optional but common
  //
  // POS varies: NOUN (most common) with nsubj/root dependencies

  r.either(
    // Pattern 1: 場合 + は (with topic particle)
    (b) => {
      const baai = b.tok({ lemmaOneOf: ['ばあい', '場合'], posOneOf: ['NOUN', 'PROPN'] }, 'baai');
      const wa = b.particle('は', 'wa');
      b.inOrder(baai, wa, 1);
      b.captureSpan('場合は', baai, wa);
    },
    // Pattern 2: 場合 without は (still valid)
    (b) => {
      const baai = b.tok({ lemmaOneOf: ['ばあい', '場合'], posOneOf: ['NOUN', 'PROPN'] }, 'baai');
      b.captureSpan('場合', baai, baai);
    }
  );
});
