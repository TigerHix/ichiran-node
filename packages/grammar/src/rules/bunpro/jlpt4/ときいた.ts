import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: ときいた - I heard that
 *
 * Matches phrases expressing hearsay - "I heard that..."
 *
 * This is the past tense of 聞く (to hear/ask) used as a quotation verb:
 * - Quote + と (quote particle)
 * - 聞く (kiku) - past tense (聞いた)
 *
 * Structures:
 * - Phrase + ときいた (casual/standard)
 * - Phrase + だときいた (after noun/na-adj)
 * - Phrase + ってきいた (colloquial quotation)
 *
 * Examples:
 * - 明日はテストがあると聞いた。
 *   (I heard that there is a test tomorrow.)
 * - これは無理だと聞いた。
 *   (I heard that this is impossible.)
 * - 友達からあなたは歌うのが上手だって聞いた。
 *   (I heard from a friend that you are good at singing.)
 *
 * Key discriminators:
 * - と (to) is a quote particle (ADP with dep=case)
 * - 聞く (kiku) is in past tense (聞いた) - lemma=聞く
 * - た (ta) is past auxiliary (AUX, lemma=た)
 *
 * GiNZA parse structure for "あると聞いた":
 * - ある (VERB/AUX, dep=ccomp) - quoted content
 * - と (ADP, dep=case, head=quoted_content) --case--> quoted_content
 * - 聞い (VERB, lemma=聞く, inflectionForm=連用形-一般)
 * - た (AUX, lemma=た, inflectionForm=終止形-一般, dep=aux, head=聞い)
 *
 * Different from:
 * - とおもう (I think) - uses 思う instead of 聞く
 * - という (called/named) - uses 言う instead of 聞く
 * - といわれている (it is said that) - uses passive progressive form
 * - Physical hearing (聞こえる) - different verb meaning
 */
export default bunproLinguisticRule('ときいた', (r) => {
  // Quote particle と (marks the quoted phrase)
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: ときいた (standard/casual form)
    // e.g., 明日はテストがあると聞いた、近くの映画館のスクリーンが一番大きいと聞いた
    (b1) => {
      // 聞く (kiku) - in past form "きいた" or "聞いた"
      // Note: In quotative constructions, both the verb (聞く) and the auxiliary (た)
      // may point to the quotative particle (と) rather than each other.
      // We rely on surface order rather than dependency structure.
      const kiku = b1.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['きく', '聞く'],
      }, 'kiku');

      // Past tense auxiliary た (ta)
      const ta = b1.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');

      // Use inOrder rather than auxOf because dependencies may point to と
      b1.inOrder(kiku, ta, 2);

      // Quote particle と comes before 聞く (within 3 tokens for typical patterns)
      b1.inOrder(to, kiku, 3);

      b1.captureSpan('ときいた', to, ta);
    },

    // Pattern 2: だときいた (after noun/na-adj)
    // e.g., これは無理だと聞いた、事故の原因はスピード違反だときいた
    (b2) => {
      // Copula だ (da)
      const da = b2.aux({
        lemma: 'だ',
        inflectionFormOneOf: ['終止形-一般', '連体形-一般'],
      }, 'da');

      // Followed by quote particle と
      b2.inOrder(da, to, 1);

      // Rest is same as Pattern 1
      const kiku = b2.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['きく', '聞く'],
      }, 'kiku');

      const ta = b2.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');

      b2.inOrder(kiku, ta, 2);
      b2.inOrder(to, kiku, 3);

      b2.captureSpan('ときいた', da, ta);
    },

    // Pattern 3: ってきいた (colloquial quotation)
    // e.g., 友達からあなたは歌うのが上手だって聞いた
    (b3) => {
      // Colloquial quote particle って (tte)
      const tte = b3.tok({
        text: 'って',
        pos: 'ADP',
        dep: 'case',
      }, 'tte');

      const kiku = b3.tok({
        posOneOf: ['VERB', 'AUX'],
        lemmaOneOf: ['きく', '聞く'],
      }, 'kiku');

      const ta = b3.aux({
        lemmaOneOf: ['た', 'だ'],
        conjugationClass: '助動詞-タ',
      }, 'ta');

      b3.inOrder(kiku, ta, 2);
      b3.inOrder(tte, kiku, 3);

      b3.captureSpan('ときいた', tte, ta);
    }
  );
});
