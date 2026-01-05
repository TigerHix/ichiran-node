import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT5: な-adjective-predicate - な-Adjective (Predicate)
 *
 * Matches な-adjectives (na-adjectives) as sentence predicates.
 * Na-adjectives require a copula (だ or です) when used as predicates.
 *
 * Pattern: na-adjective + だ/です (copula)
 *
 * Both casual and polite forms:
 * - Casual: na-adj + だ (e.g., 静かだ, 元気だ, きれいだ)
 * - Polite: na-adj + です (e.g., 静かです, 元気です, きれいです)
 *
 * The copula can be followed by question particle か, but not by other
 * auxiliaries like た (past) or ない (negative).
 *
 * GiNZA parses na-adjectives as:
 * - Stem: pos=ADJ or VERB, tag=形状詞-一般 or 名詞-普通名詞-形状詞可能, dep=root
 * - Copula だ: pos=AUX, lemma=だ, tag=助動詞, dep=aux, conjugationClass=助動詞-ダ, inflectionForm=終止形-一般
 * - Copula です: pos=AUX, lemma=です, tag=助動詞, dep=aux, conjugationClass=助動詞-デス, inflectionForm=終止形-一般
 *
 * NOTE: Hiragana na-adjectives (e.g., べんり) are parsed as pos=VERB, while kanji
 * na-adjectives (e.g., 便利) are parsed as pos=ADJ. Both must be matched.
 *
 * This rule captures the entire predicate form including the copula.
 */
export default bunproLinguisticRule('な-adjective-predicate', (r) => {
  r.either(
    // ========== CASUAL FORM: na-adjective + だ ==========

    // 1. 形状詞-一般 + だ (e.g., 静かだ, 綺麗だ, 簡単だ)
    (b) => {
      const naAdj = b.adj({
        tag: '形状詞-一般',
        dep: 'root',
      }, 'naAdj');

      const da = b.aux({
        lemma: 'だ',
        conjugationClass: '助動詞-ダ',
        inflectionForm: '終止形-一般',
      }, 'da');

      b.auxOf(naAdj, da);
      b.captureSpan('match', naAdj, da);
    },

    // 2. 名詞-普通名詞-形状詞可能 (pos=ADJ) + だ (e.g., 元気だ, 便利だ)
    (b) => {
      const naAdj = b.adj({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const da = b.aux({
        lemma: 'だ',
        conjugationClass: '助動詞-ダ',
        inflectionForm: '終止形-一般',
      }, 'da');

      b.auxOf(naAdj, da);
      b.captureSpan('match', naAdj, da);
    },

    // 3. 名詞-普通名詞-形状詞可能 (pos=VERB) + だ (e.g., べんりだ, ひまだ)
    (b) => {
      const naAdj = b.verb({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const da = b.aux({
        lemma: 'だ',
        conjugationClass: '助動詞-ダ',
        inflectionForm: '終止形-一般',
      }, 'da');

      b.auxOf(naAdj, da);
      b.captureSpan('match', naAdj, da);
    },

    // ========== CASUAL FORM WITH QUESTION: na-adjective + だ + か ==========

    // 4. 形状詞-一般 + だ + か
    (b) => {
      const naAdj = b.adj({
        tag: '形状詞-一般',
        dep: 'root',
      }, 'naAdj');

      const da = b.aux({
        lemma: 'だ',
        conjugationClass: '助動詞-ダ',
        inflectionForm: '終止形-一般',
      }, 'da');

      const ka = b.particle('か', 'ka');

      b.auxOf(naAdj, da);
      b.inOrder(da, ka, 1);
      b.captureSpan('match', naAdj, ka);
    },

    // 5. 名詞-普通名詞-形状詞可能 (pos=ADJ) + だ + か
    (b) => {
      const naAdj = b.adj({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const da = b.aux({
        lemma: 'だ',
        conjugationClass: '助動詞-ダ',
        inflectionForm: '終止形-一般',
      }, 'da');

      const ka = b.particle('か', 'ka');

      b.auxOf(naAdj, da);
      b.inOrder(da, ka, 1);
      b.captureSpan('match', naAdj, ka);
    },

    // 6. 名詞-普通名詞-形状詞可能 (pos=VERB) + だ + か
    (b) => {
      const naAdj = b.verb({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const da = b.aux({
        lemma: 'だ',
        conjugationClass: '助動詞-ダ',
        inflectionForm: '終止形-一般',
      }, 'da');

      const ka = b.particle('か', 'ka');

      b.auxOf(naAdj, da);
      b.inOrder(da, ka, 1);
      b.captureSpan('match', naAdj, ka);
    },

    // ========== POLITE FORM: na-adjective + です ==========

    // 7. 形状詞-一般 + です (e.g., 静かです, 綺麗です, 簡単です)
    (b) => {
      const naAdj = b.adj({
        tag: '形状詞-一般',
        dep: 'root',
      }, 'naAdj');

      const desu = b.aux({
        lemma: 'です',
        conjugationClass: '助動詞-デス',
        inflectionForm: '終止形-一般',
      }, 'desu');

      b.auxOf(naAdj, desu);
      b.captureSpan('match', naAdj, desu);
    },

    // 8. 名詞-普通名詞-形状詞可能 (pos=ADJ) + です (e.g., 元気です, 便利です)
    (b) => {
      const naAdj = b.adj({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const desu = b.aux({
        lemma: 'です',
        conjugationClass: '助動詞-デス',
        inflectionForm: '終止形-一般',
      }, 'desu');

      b.auxOf(naAdj, desu);
      b.captureSpan('match', naAdj, desu);
    },

    // 9. 名詞-普通名詞-形状詞可能 (pos=VERB) + です (e.g., べんりです, ひまです)
    (b) => {
      const naAdj = b.verb({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const desu = b.aux({
        lemma: 'です',
        conjugationClass: '助動詞-デス',
        inflectionForm: '終止形-一般',
      }, 'desu');

      b.auxOf(naAdj, desu);
      b.captureSpan('match', naAdj, desu);
    },

    // ========== POLITE FORM WITH QUESTION: na-adjective + です + か ==========

    // 10. 形状詞-一般 + です + か
    (b) => {
      const naAdj = b.adj({
        tag: '形状詞-一般',
        dep: 'root',
      }, 'naAdj');

      const desu = b.aux({
        lemma: 'です',
        conjugationClass: '助動詞-デス',
        inflectionForm: '終止形-一般',
      }, 'desu');

      const ka = b.particle('か', 'ka');

      b.auxOf(naAdj, desu);
      b.inOrder(desu, ka, 1);
      b.captureSpan('match', naAdj, ka);
    },

    // 11. 名詞-普通名詞-形状詞可能 (pos=ADJ) + です + か
    (b) => {
      const naAdj = b.adj({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const desu = b.aux({
        lemma: 'です',
        conjugationClass: '助動詞-デス',
        inflectionForm: '終止形-一般',
      }, 'desu');

      const ka = b.particle('か', 'ka');

      b.auxOf(naAdj, desu);
      b.inOrder(desu, ka, 1);
      b.captureSpan('match', naAdj, ka);
    },

    // 12. 名詞-普通名詞-形状詞可能 (pos=VERB) + です + か
    (b) => {
      const naAdj = b.verb({
        tag: '名詞-普通名詞-形状詞可能',
        dep: 'root',
      }, 'naAdj');

      const desu = b.aux({
        lemma: 'です',
        conjugationClass: '助動詞-デス',
        inflectionForm: '終止形-一般',
      }, 'desu');

      const ka = b.particle('か', 'ka');

      b.auxOf(naAdj, desu);
      b.inOrder(desu, ka, 1);
      b.captureSpan('match', naAdj, ka);
    }
  );
});
