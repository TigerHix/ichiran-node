import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: といわれている - It is said that / Is known as
 *
 * Matches phrases expressing hearsay or general opinion - "it is said that..."
 *
 * This is the passive progressive form of 言う (to say):
 * - Quote + と (quote particle)
 * - 言う (iu) - passive form (言われる)
 * - ている (iru) - progressive aspect
 *
 * Structures:
 * - Phrase + といわれている (casual/standard)
 * - Phrase + だといわれている (after noun/na-adj)
 * - Phrase + といわれてる (colloquial contraction)
 * - Phrase + だといわれてる (colloquial with だ)
 *
 * Examples:
 * - 危ないと言われているので、しないほうがいいよ。
 *   (Because it is said that it's dangerous, you shouldn't do it.)
 * - ライオンがジャングルの王様だと言われている。
 *   (It is said that lions are the kings of the jungle.)
 * - 大阪のタコ焼きは日本一美味しいといわれている。
 *   (Osaka's takoyaki is said to be the most delicious in Japan.)
 *
 * Key discriminators:
 * - と (to) is a quote particle (ADP with dep=case)
 * - 言う (iu) is in passive form (言われる) - lemma=言う, inflectionForm=未然形-一般
 * - れる (reru) is passive auxiliary (AUX, lemma=れる)
 * - て (te) is te-form connector (SCONJ, dep=mark)
 * - いる (iru) is progressive auxiliary (VERB/AUX, lemma=いる)
 *
 * GiNZA parse structure for "危ないと言われている":
 * - 危ない (ADJ, dep=ccomp) - quoted content
 * - と (ADP, dep=case, head=quoted_content) --case--> quoted_content
 * - 言わ (VERB, lemma=言う, inflectionForm=未然形-一般)
 * - れ (AUX, lemma=れる, inflectionForm=連用形-一般, dep=aux, head=言わ)
 * - て (SCONJ, dep=mark, head=言わ)
 * - いる (VERB, lemma=いる, inflectionForm=終止形-一般, dep=fixed, head=て)
 *
 * Different from:
 * - ということだ (JLPT3) - "it means that" (uses いう + こと + だ)
 * - とされている (JLPT4) - "it is considered that" (uses される instead of 言われている)
 * - という (JLPT3) - "called/named" (no auxiliary verbs)
 */
export default linguisticRule('といわれている', (r) => {
  // Quote particle と (marks the quoted phrase)
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: といわれている (standard/casual form)
    // e.g., 危ないと言われている、大阪のタコ焼きは日本一美味しいといわれている
    (b1) => {
      // 言う (iu) in passive form (未然形-一般)
      // GiNZA may lemmatize as either いう (hiragana) or 言う (kanji)
      const iu = b1.verb({
        lemmaOneOf: ['いう', '言う'],
        inflectionForm: '未然形-一般',
      }, 'iu');

      // Passive auxiliary れる (reru) in 連用形-一般
      const reru = b1.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      // れる attaches to 言う as aux
      b1.auxOf(iu, reru);

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

      // Quote particle と comes before 言う
      b1.inOrder(to, iu, 3);
      b1.inOrder(iu, reru, 1);
      b1.inOrder(reru, te, 1);
      b1.inOrder(te, iru, 1);

      b1.captureSpan('といわれている', to, iru);
    },

    // Pattern 2: だといわれている (after noun/na-adj)
    // e.g., ライオンがジャングルの王様だと言われている
    (b2) => {
      // Copula だ (da)
      const da = b2.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      // Followed by quote particle と
      b2.inOrder(da, to, 1);

      // Rest is same as Pattern 1
      const iu = b2.verb({
        lemmaOneOf: ['いう', '言う'],
        inflectionForm: '未然形-一般',
      }, 'iu');

      const reru = b2.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b2.auxOf(iu, reru);

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

      b2.inOrder(to, iu, 3);
      b2.inOrder(iu, reru, 1);
      b2.inOrder(reru, te, 1);
      b2.inOrder(te, iru, 1);

      b2.captureSpan('といわれている', da, iru);
    },

    // Pattern 3: といわれてる (colloquial contraction - てる instead of ている)
    // e.g., 危ないといわれてる、あの人の家では幽霊が出るといわれてる
    (b3) => {
      const iu = b3.verb({
        lemmaOneOf: ['いう', '言う'],
        inflectionForm: '未然形-一般',
      }, 'iu');

      const reru = b3.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b3.auxOf(iu, reru);

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

      b3.inOrder(to, iu, 3);
      b3.inOrder(iu, reru, 1);
      b3.inOrder(reru, te, 1);
      b3.inOrder(te, teru, 1);

      b3.captureSpan('といわれている', to, teru);
    },

    // Pattern 4: だといわれてる (colloquial with だ)
    // e.g., 王様だといわれてる
    (b4) => {
      const da = b4.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      b4.inOrder(da, to, 1);

      const iu = b4.verb({
        lemmaOneOf: ['いう', '言う'],
        inflectionForm: '未然形-一般',
      }, 'iu');

      const reru = b4.aux({
        lemma: 'れる',
        inflectionForm: '連用形-一般',
      }, 'reru');

      b4.auxOf(iu, reru);

      const te = b4.tok({
        text: 'て',
        pos: 'SCONJ',
        dep: 'mark',
      }, 'te');

      const teru = b4.tok({
        text: 'てる',
        posOneOf: ['VERB', 'AUX'],
      }, 'teru');

      b4.inOrder(to, iu, 3);
      b4.inOrder(iu, reru, 1);
      b4.inOrder(reru, te, 1);
      b4.inOrder(te, teru, 1);

      b4.captureSpan('といわれている', da, teru);
    }
  );
});
