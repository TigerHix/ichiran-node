import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: っこない (kkonai) - There is no chance of, impossible
 *
 * A colloquial emphatic negative expression meaning "there's no way" or "impossible".
 * Attached to verb stems (often potential verbs) to express strong certainty that
 * something cannot or will not happen.
 *
 * Structure: Verb［stem］+ っこ(ない/ありません)
 *
 * Examples:
 * - できっこない (no way to do it)
 * - わかりっこない (no way to understand)
 * - 勝てっこない (no way to win)
 * - 読めっこない (no way to read)
 * - 覚えられっこない (no way to remember)
 * - 取れっこない (no way to take)
 * - 書けっこない (no way to write)
 * - 買えっこない (no way to buy)
 * - ほろぼせっこありません (polite: no way to destroy)
 *
 * Key discriminators:
 * - Follows verb stem (連用形 - conjunctive form)
 * - っこ is a suffix (POS=AUX/VERB/NOUN, tag=接尾辞-名詞的-一般)
 * - Followed by ない (casual) or ありません (polite)
 * - Often used with potential verbs (〜れる, 〜できる)
 * - Colloquial and emphatic - stronger than simple ない
 *
 * GiNZA parse patterns:
 * 1. Verb stem (連用形-一般) + っこ (AUX/VERB, dep=aux/root) + ない (AUX, dep=aux)
 * 2. Verb stem + っこ (NOUN, dep=obl) + あり (VERB) + ませ (AUX) [polite form]
 * 3. Potential verb stem + っこ + ない (e.g., 勝てっこない)
 * 4. Various verb forms: 五段 verbs, 一段 verbs, irregular verbs
 *
 * Different from:
 * - Simple negative ない (weaker, less emphatic)
 * - そうにない (unlikely to, showing no signs of - weaker)
 * - わけがない (no reason to, can't be - more logical)
 * - ようがない (no way to do - no method)
 */
export default bunproLinguisticRule('っこない', (r) => {
  r.either(
    // Branch 1: Verb stem + っこ (AUX) + ない (AUX)
    // Example: できっこない
    // Parse: でき(VERB) + っこ(AUX,dep=aux) + ない(AUX,dep=aux)
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kko = b.aux({
        text: 'っこ',
        tag: '接尾辞-名詞的-一般',
      }, 'kko');
      b.auxOf(stem, kko);
      const nai = b.aux({
        lemma: 'ない',
        tag: '形容詞-非自立可能',
      }, 'nai');
      b.auxOf(stem, nai);
      b.captureSpan('っこない', stem, nai);
    },

    // Branch 2: Verb stem + っこ (VERB) + ない (AUX)
    // Example: わかりっこない
    // Parse: わかり(VERB) + っこ(VERB,dep=root) + ない(AUX,dep=aux)
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kko = b.tok({
        text: 'っこ',
        pos: 'VERB',
        tag: '接尾辞-名詞的-一般',
      }, 'kko');
      b.inOrder(stem, kko, 3);
      const nai = b.aux({
        lemma: 'ない',
        tag: '形容詞-非自立可能',
      }, 'nai');
      b.auxOf(kko, nai);
      b.captureSpan('っこない', stem, nai);
    },

    // Branch 3: Verb stem + っこ (NOUN) + あり (VERB) + ませ (AUX) [polite form]
    // Example: ほろぼせっこありません
    // Parse: ほろぼせ(VERB) + っこ(NOUN,dep=obl) + あり(VERB) + ませ(AUX)
    (b) => {
      const stem = b.verb({
        inflectionForm: '連用形-一般',
      }, 'stem');
      const kko = b.tok({
        text: 'っこ',
        pos: 'NOUN',
        tag: '接尾辞-名詞的-一般',
      }, 'kko');
      b.inOrder(stem, kko, 3);
      const ari = b.verb({
        lemma: 'ある',
        inflectionForm: '連用形-一般',
      }, 'ari');
      b.inOrder(kko, ari, 3);
      const mase = b.aux({
        lemma: 'ます',
      }, 'mase');
      b.auxOf(ari, mase);
      b.captureSpan('っこない', stem, mase);
    },

    // Branch 4: Flexible pattern - any verb + っこ + ない
    // Example: 勝てっこない, 読めっこない (potential verbs)
    // Example: なりっこない, なしとげっこない (various verb forms)
    (b) => {
      const stem = b.verb({}, 'stem');
      const kko = b.tok({
        text: 'っこ',
        tag: '接尾辞-名詞的-一般',
      }, 'kko');
      b.inOrder(stem, kko, 5);
      const nai = b.aux({
        lemma: 'ない',
        tag: '形容詞-非自立可能',
      }, 'nai');
      b.inOrder(kko, nai, 5);
      b.captureSpan('っこない', stem, nai);
    },

    // Branch 5: Most flexible - verb + っこ + ない (no constraints on っこ)
    // Catch-all for various GiNZA parsing variations
    (b) => {
      const stem = b.verb({}, 'stem');
      const kko = b.tok({
        text: 'っこ',
      }, 'kko');
      b.inOrder(stem, kko, 5);
      const nai = b.tok({
        lemma: 'ない',
      }, 'nai');
      b.inOrder(kko, nai, 5);
      b.captureSpan('っこない', stem, nai);
    }
  );
});
