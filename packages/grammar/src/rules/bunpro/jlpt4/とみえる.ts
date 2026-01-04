import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: とみえる (it appears that / it seems that / one can conclude that)
 *
 * Matches quote particle と + 見える (to appear/seem based on visual evidence)
 *
 * This grammar point expresses a conclusion or conjecture based on how something
 * appears visually. It's more objective/formal than ようだ and implies drawing
 * a conclusion from specific evidence.
 *
 * Structure:
 * - Verb (short form) + と + みえる/みえる/みえて
 * - [い]Adjective + と + みえる/みえる/みえて
 * - [な]Adjective + (だ) + と + みえる/みえる/みえて
 * - Noun + (だ) + と + みえる/みえる/みえて
 *
 * Examples:
 * - 暑いとみえる (seems hot - he took off his jacket)
 * - 嫌いだとみえる (seems to dislike - left the sandwich)
 * - 留守とみえた (seemed no one was home)
 * - 雨が降ったとみえて (it appears it rained - laundry is soaking wet)
 *
 * This is different from:
 * - にみえる - describes visual appearance (looks hot, looks young)
 * - ようだ - subjective conjecture, less formal
 * - そうだ - hearsay (I heard that...)
 * - らしい - hearsay or general impression
 *
 * GiNZA parse structure:
 * - と is ADP/PART with dep=case (quotative particle)
 * - 見える is VERB with various inflection forms (終止形, 連用形-て, 連体形-タ)
 */
export default linguisticRule('とみえる', (r) => {
  // Quote particle と (marks the quoted content/conclusion)
  const to = r.particle('と', 'to');

  r.either(
    // Pattern 1: Present/casual form (とみえる)
    // e.g., 暑いとみえる, 知っていたとみえる
    (b1) => {
      const mieru = b1.verb({
        lemma: '見える',
        inflectionForm: '終止形-一般',
      }, 'mieru');

      b1.inOrder(to, mieru, 1);
      b1.captureSpan('とみえる', to, mieru);
    },

    // Pattern 2: Te-form (とみえて) - connecting two clauses
    // e.g., 暑いとみえて、汗をかいている, 難しかったとみえて
    (b2) => {
      const miete = b2.verb({
        lemma: '見える',
        inflectionForm: '連用形-て',
      }, 'mieru');

      b2.inOrder(to, miete, 1);
      b2.captureSpan('とみえる', to, miete);
    },

    // Pattern 3: Past tense (とみえた)
    // e.g., 留守とみえた, 驚いたとみえる
    (b3) => {
      const mieta = b3.verb({
        lemma: '見える',
        inflectionFormOneOf: ['連体形-タ', '終止形-タ'],
      }, 'mieru');

      b3.inOrder(to, mieta, 1);
      b3.captureSpan('とみえる', to, mieta);
    },

    // Pattern 4: Polite form (とみえます)
    // e.g., 暑いとみえます, 静かだとみえます
    (b4) => {
      const mieru = b4.verb({
        lemma: '見える',
        inflectionForm: '連用形-一般',
      }, 'mieru');
      const masu = b4.aux({
        lemma: 'ます',
        dep: 'aux',
      }, 'masu');

      b4.inOrder(to, mieru, 1);
      b4.auxOf(mieru, masu);
      b4.inOrder(mieru, masu, 1);
      b4.captureSpan('とみえる', to, masu);
    },

    // Pattern 5: Te-form polite (とみえてまして)
    // e.g., 難しかったとみえてまして
    (b5) => {
      const miete = b5.verb({
        lemma: '見える',
        inflectionForm: '連用形-て',
      }, 'mieru');
      const mashite = b5.aux({
        lemma: 'ます',
        dep: 'aux',
        inflectionForm: '連用形-イ音便',
      }, 'mashite');

      b5.inOrder(to, miete, 1);
      b5.inOrder(miete, mashite, 1);
      b5.captureSpan('とみえる', to, mashite);
    }
  );
});
