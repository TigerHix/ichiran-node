import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: とかんがえられている - It is considered that / Is thought to be
 *
 * Matches phrases expressing "it is considered that..." or "it is thought that..."
 *
 * This is the passive progressive form of 考える (to think/consider):
 * - Quote + と (quote particle)
 * - 考える (kangaeru) - passive form (考えられる)
 * - ている (iru) - progressive aspect
 *
 * Also accepts alternate verb 思う (omou) - "to think"
 *
 * Structures:
 * - Phrase + とかんがえられている (casual/standard)
 * - Phrase + だとかんがえられている (after noun/na-adj)
 * - Phrase + とおもわれている (alternate verb 思う)
 * - Phrase + だとおもわれている (with だ)
 *
 * Examples:
 * - お金はどんな人にも悪い影響があるとかんがえられている。
 *   (Money is considered to have a bad influence on anyone.)
 * - この建物は５００年頃建てられたと考えられている。
 *   (This building is thought to have been built around the year 500.)
 * - 英語は世界中で使われる言語だと考えられている。
 *   (English is considered to be a worldly language.)
 * - 日本語は最も難しい言語の一つとおもわれている。
 *   (Japanese is thought of as one of the most difficult languages.)
 *
 * Key discriminators:
 * - と (to) is a quote particle (ADP with dep=case)
 * - 考える (kangaeru) or 思う (omou) in passive form
 *   - For 考える: lemma=考える, inflectionForm=未然形-一般
 *   - For 思う: lemma=思う, inflectionForm=未然形-一般
 * - られる (rareru) is passive auxiliary (AUX, lemma=られる)
 * - て (te) is te-form connector (SCONJ, dep=mark)
 * - いる (iru) is progressive auxiliary (VERB/AUX, lemma=いる)
 *
 * GiNZA parse structure for "悪い影響があるとかんがえられている":
 * - 悪い影響がある (quoted content)
 * - と (ADP, dep=case, head=quoted_content) --case--> quoted_content
 * - 考え (VERB, lemma=考える, inflectionForm=未然形-一般)
 * - られ (AUX, lemma=られる, inflectionForm=連用形-一般, dep=aux, head=考え)
 * - て (SCONJ, dep=mark, head=考え)
 * - いる (VERB, lemma=いる, inflectionForm=終止形-一般, dep=fixed, head=て)
 *
 * Different from:
 * - といわれている (JLPT4) - "it is said that" (uses 言う instead of 考える/思う)
 * - とされている (JLPT4) - "it is considered that" (uses する instead of 考える/思う)
 * - と考えられる (JLPT2) - "it can be considered" (potential form, not progressive)
 */
export default linguisticRule('とかんがえられている', (r) => {
  // Quote particle と (marks the quoted phrase)
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: とかんがえられている (standard/casual form with 考える)
    // e.g., 悪い影響があるとかんがえられている、この建物は500年頃建てられたと考えられている
    (b1) => {
      // 考える (kangaeru) in passive form (未然形-一般)
      const kangaeru = b1.verb({
        lemma: 'かんがえる',
        inflectionForm: '未然形-一般',
      }, 'kangaeru');

      // Passive auxiliary られる (rareru) in 連用形-一般
      const rareru = b1.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'rareru');

      // られる attaches to 考える as aux
      b1.auxOf(kangaeru, rareru);

      // Followed by te-form connector て
      const te = b1.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      // Followed by いる (iru) - progressive aspect
      // Can be 終止形-一般 (sentence-final) or 連体形-一般 (before nouns)
      const iru = b1.tok({
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'iru');

      // いる attaches as fixed to て
      b1.headChild(te, iru, 'fixed');

      // Quote particle と comes before 考える
      b1.inOrder(to, kangaeru, 3);
      b1.inOrder(kangaeru, rareru, 1);
      b1.inOrder(rareru, te, 1);
      b1.inOrder(te, iru, 1);

      b1.captureSpan('とかんがえられている', to, iru);
    },

    // Pattern 2: だとかんがえられている (after noun/na-adj)
    // e.g., 英語は世界中で使われる言語だと考えられている
    (b2) => {
      // Copula だ (da)
      const da = b2.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      // Followed by quote particle と
      b2.inOrder(da, to, 1);

      // Rest is same as Pattern 1
      const kangaeru = b2.verb({
        lemma: 'かんがえる',
        inflectionForm: '未然形-一般',
      }, 'kangaeru');

      const rareru = b2.aux({
        lemma: 'られる',
        inflectionForm: '連用形-一般',
      }, 'rareru');

      b2.auxOf(kangaeru, rareru);

      const te = b2.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const iru = b2.tok({
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'iru');

      b2.headChild(te, iru, 'fixed');

      b2.inOrder(to, kangaeru, 3);
      b2.inOrder(kangaeru, rareru, 1);
      b2.inOrder(rareru, te, 1);
      b2.inOrder(te, iru, 1);

      b2.captureSpan('とかんがえられている', da, iru);
    },

    // Pattern 3: とおもわれている (alternate verb 思う)
    // e.g., 日本語は最も難しい言語の一つとおもわれている
    (b3) => {
      // 思う (omou) in passive form (未然形-一般)
      const omou = b3.verb({
        lemma: 'おもう',
        inflectionForm: '未然形-一般',
      }, 'omou');

      // Passive auxiliary れる (reru) in 連用形-一般
      // Note: GiNZA analyzes おもわ (stem of おもう) + れ (aux, lemma=れる)
      const wareru = b3.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'wareru');

      // われる attaches to 思う as aux
      b3.auxOf(omou, wareru);

      // Followed by te-form connector て
      const te = b3.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      // Followed by いる (iru) - progressive aspect
      const iru = b3.tok({
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'iru');

      // いる attaches as fixed to て
      b3.headChild(te, iru, 'fixed');

      // Quote particle と comes before 思う
      b3.inOrder(to, omou, 3);
      b3.inOrder(omou, wareru, 1);
      b3.inOrder(wareru, te, 1);
      b3.inOrder(te, iru, 1);

      b3.captureSpan('とかんがえられている', to, iru);
    },

    // Pattern 4: だとおもわれている (with だ)
    // e.g., サメが最も危ない動物だとおもわれている
    (b4) => {
      // Copula だ (da)
      const da = b4.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      // Followed by quote particle と
      b4.inOrder(da, to, 1);

      // Rest is same as Pattern 3
      const omou = b4.verb({
        lemma: 'おもう',
        inflectionForm: '未然形-一般',
      }, 'omou');

      const wareru = b4.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'wareru');

      b4.auxOf(omou, wareru);

      const te = b4.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const iru = b4.tok({
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'iru');

      b4.headChild(te, iru, 'fixed');

      b4.inOrder(to, omou, 3);
      b4.inOrder(omou, wareru, 1);
      b4.inOrder(wareru, te, 1);
      b4.inOrder(te, iru, 1);

      b4.captureSpan('とかんがえられている', da, iru);
    }
  );
});
