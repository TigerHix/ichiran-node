import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: とされている - It is considered that / Is regarded as
 *
 * Matches phrases expressing general opinion or accepted belief - "it is considered that..."
 *
 * This is the passive progressive form of する (to do/regard):
 * - Quote + と (quote particle)
 * - する (suru) - passive form (される)
 * - ている (iru) - progressive aspect
 *
 * Structures:
 * - Phrase + とされている (casual/standard)
 * - Phrase + だとされている (after noun/na-adj)
 * - Phrase + とされてる (colloquial contraction)
 * - Phrase + だとされてる (colloquial with だ)
 *
 * Examples:
 * - インターネットが必要だとされている。
 *   (The internet is considered to be necessary.)
 * - この神社は最古のものとされている。
 *   (This shrine is considered to be the oldest one.)
 * - 日本人は議論が苦手とされている。
 *   (Japanese are considered to be poor debaters.)
 *
 * Key discriminators:
 * - と (to) is a quote particle (ADP with dep=case)
 * - する (suru) is in passive form (される) - lemma=する, inflectionForm=未然形-一般
 * - れる (reru) is passive auxiliary (AUX, lemma=れる)
 * - て (te) is te-form connector (SCONJ, dep=mark)
 * - いる (iru) is progressive auxiliary (VERB/AUX, lemma=いる)
 *
 * GiNZA parse structure for "必要だとされている":
 * - 必要 (NOUN/ADJ) - quoted content
 * - だ (AUX, lemma=だ) - copula
 * - と (ADP, dep=case, head=quoted_content) --case--> quoted_content
 * - さ (VERB, lemma=する, inflectionForm=未然形-一般)
 * - れ (AUX, lemma=れる, inflectionForm=連用形-一般, dep=aux, head=さ)
 * - て (SCONJ, dep=mark, head=さ)
 * - いる (VERB, lemma=いる, inflectionForm=終止形-一般, dep=fixed, head=て)
 *
 * Different from:
 * - といわれている (JLPT4) - "it is said that" (uses 言う instead of する)
 * - とかんがえられている (JLPT4) - "is thought of as" (uses 考える)
 * - ということだ (JLPT3) - "it means that" (uses いう + こと + だ)
 */
export default bunproLinguisticRule('とされている', (r) => {
  // Quote particle と (marks the quoted phrase)
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: とされている (standard/casual form)
    // e.g., 重要とされている、議論が苦手とされている
    (b1) => {
      // する (suru) in passive form (未然形-一般 or 未然形-サ)
      const suru = b1.verb({
        lemma: 'する',
        inflectionFormOneOf: ['未然形-一般', '未然形-サ'],
      }, 'suru');

      // Passive auxiliary れる (reru) in 連用形-一般
      const reru = b1.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      // れる attaches to する as aux
      b1.auxOf(suru, reru);

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

      // Quote particle と comes before する
      b1.inOrder(to, suru, 3);
      b1.inOrder(suru, reru, 1);
      b1.inOrder(reru, te, 1);
      b1.inOrder(te, iru, 1);

      b1.captureSpan('とされている', to, iru);
    },

    // Pattern 1.5: とされていた (past progressive, without だ)
    // e.g., 重要とされていた
    (b15) => {
      const suru = b15.verb({
        lemma: 'する',
        inflectionFormOneOf: ['未然形-一般', '未然形-サ'],
      }, 'suru');

      const reru = b15.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b15.auxOf(suru, reru);

      const te = b15.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const ita = b15.tok({
        text: 'い',
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'ita');

      const ta = b15.aux({
        text: 'た',
      }, 'ta');

      b15.inOrder(to, suru, 3);
      b15.inOrder(suru, reru, 1);
      b15.inOrder(reru, te, 1);
      b15.inOrder(te, ita, 1);
      b15.inOrder(ita, ta, 1);

      b15.captureSpan('とされている', to, ta);
    },

    // Pattern 2: だとされている (after noun/na-adj)
    // e.g., 必要だとされている、王様だとされている
    (b2) => {
      // Copula だ (da)
      const da = b2.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      // Followed by quote particle と
      b2.inOrder(da, to, 1);

      // Rest is same as Pattern 1
      const suru = b2.verb({
        lemma: 'する',
        inflectionFormOneOf: ['未然形-一般', '未然形-サ'],
      }, 'suru');

      const reru = b2.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b2.auxOf(suru, reru);

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

      b2.inOrder(to, suru, 3);
      b2.inOrder(suru, reru, 1);
      b2.inOrder(reru, te, 1);
      b2.inOrder(te, iru, 1);

      b2.captureSpan('とされている', da, iru);
    },

    // Pattern 2.5: とされていた (past progressive)
    // e.g., 悪魔の音楽とされていた、女のものだとされていた
    (b25) => {
      const da = b25.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      b25.inOrder(da, to, 1);

      const suru = b25.verb({
        lemma: 'する',
        inflectionFormOneOf: ['未然形-一般', '未然形-サ'],
      }, 'suru');

      const reru = b25.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b25.auxOf(suru, reru);

      const te = b25.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      // Past progressive: いて + た (いる in 連用形-一般 + た)
      const ita = b25.tok({
        text: 'い',
        lemma: 'いる',
        posOneOf: ['VERB', 'AUX'],
        inflectionForm: '連用形-一般',
      }, 'ita');

      const ta = b25.aux({
        text: 'た',
      }, 'ta');

      b25.inOrder(to, suru, 3);
      b25.inOrder(suru, reru, 1);
      b25.inOrder(reru, te, 1);
      b25.inOrder(te, ita, 1);
      b25.inOrder(ita, ta, 1);

      b25.captureSpan('とされている', da, ta);
    },

    // Pattern 3: とされてる (colloquial contraction - てる instead of ている)
    // e.g., 重要とされてる、一番とされてる
    (b3) => {
      const suru = b3.verb({
        lemma: 'する',
        inflectionFormOneOf: ['未然形-一般', '未然形-サ'],
      }, 'suru');

      const reru = b3.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b3.auxOf(suru, reru);

      const te = b3.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      // Colloquial form てる (contracted from ている)
      const teru = b3.tok({
        text: 'てる',
        posOneOf: ['VERB', 'AUX'],
      }, 'teru');

      b3.inOrder(to, suru, 3);
      b3.inOrder(suru, reru, 1);
      b3.inOrder(reru, te, 1);
      b3.inOrder(te, teru, 1);

      b3.captureSpan('とされている', to, teru);
    },

    // Pattern 4: だとされてる (colloquial with だ)
    // e.g., 必要だとされてる
    (b4) => {
      const da = b4.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      b4.inOrder(da, to, 1);

      const suru = b4.verb({
        lemma: 'する',
        inflectionFormOneOf: ['未然形-一般', '未然形-サ'],
      }, 'suru');

      const reru = b4.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b4.auxOf(suru, reru);

      const te = b4.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const teru = b4.tok({
        text: 'てる',
        posOneOf: ['VERB', 'AUX'],
      }, 'teru');

      b4.inOrder(to, suru, 3);
      b4.inOrder(suru, reru, 1);
      b4.inOrder(reru, te, 1);
      b4.inOrder(te, teru, 1);

      b4.captureSpan('とされている', da, teru);
    }
  );
});
