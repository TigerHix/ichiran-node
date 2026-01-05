import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: かのようだ (ka no you da) - As if it were, looks like, seems like
 *
 * Expresses that something appears to be or seems like something else, with
 * a sense of uncertainty or figurative comparison. The combination of
 * question particle か + nominalizer の + ようだ creates a more
 * hypothetical/figurative nuance than just ようだ.
 *
 * Structures:
 * - Verb[plain] + か + の + ようだ/です
 * - ［い］Adj + か + の + ようだ/です
 * - ［な］Adj + である + か + の + ようだ/です
 * - Noun + である + か + の + ようだ/です
 *
 * Variations:
 * - かのようだ (casual)
 * - かのようです (polite)
 * - かのようだった (past casual)
 * - かのようでした (past polite)
 * - かのように (adverbial - modifying verbs)
 * - かのような (adnominal - modifying nouns)
 *
 * Examples:
 * - まるで本物の飛行機を操縦しているかのようだ。
 *   (It's just as if I'm flying an actual airplane.)
 * - この町には誰もいないかのようだ。
 *   (It's as if there's no one in this town.)
 * - 彼はまるでお腹に風船を入れているかのようだ。
 *   (He looks just as if he has a balloon in his belly.)
 *
 * Key discriminators:
 * - か (question particle) adds uncertainty/figurative nuance
 * - の (nominalizer) connects to ようだ
 * - Must distinguish from:
 *   - ようだ/ようです (without かの - less hypothetical)
 *   - かのように (adverbial use - modifies verb)
 *   - かのような (adnominal use - modifies noun)
 *
 * GiNZA parse structure:
 * - Preceding predicate + か (PART, dep=mark) + の (PART/SCONJ) + よう (AUX) + だ (AUX)
 * - Often combined with まるで (marude - "just as if") for emphasis
 */
export default bunproLinguisticRule('かのようだ', (r) => {
  r.either(
    // Branch 1: Verb/plain form + か + の + ようだ (sentence-final)
    // e.g., 操縦しているかのようだ
    (b1) => {
      const ka = b1.particle('か', 'ka');
      const no = b1.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b1.aux({ lemma: 'よう' }, 'you');
      const da = b1.tok({ text: 'だ', lemma: 'だ' }, 'da');

      b1.inOrder(ka, no, 2);
      b1.inOrder(no, you, 2);
      b1.auxOf(you, da);

      b1.captureSpan('かのようだ', ka, da);
    },

    // Branch 2: Verb/plain form + か + の + ようです (polite)
    // e.g., 操縦しているかのようです
    (b2) => {
      const ka = b2.particle('か', 'ka');
      const no = b2.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b2.aux({ lemma: 'よう' }, 'you');
      const desu = b2.aux({ lemma: 'です' }, 'desu');

      b2.inOrder(ka, no, 2);
      b2.inOrder(no, you, 2);
      b2.auxOf(you, desu);

      b2.captureSpan('かのようだ', ka, desu);
    },

    // Branch 3: Verb/plain form + か + の + ようだった (past casual)
    // e.g., 操縦していたかのようだった
    (b3) => {
      const ka = b3.particle('か', 'ka');
      const no = b3.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b3.aux({ lemma: 'よう' }, 'you');
      const datta = b3.aux({ text: 'だった', lemma: 'だ' }, 'datta');

      b3.inOrder(ka, no, 2);
      b3.inOrder(no, you, 2);
      b3.auxOf(you, datta);

      b3.captureSpan('かのようだ', ka, datta);
    },

    // Branch 4: Verb/plain form + か + の + ようでした (past polite)
    // e.g., 操縦していたかのようでした
    (b4) => {
      const ka = b4.particle('か', 'ka');
      const no = b4.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b4.aux({ lemma: 'よう' }, 'you');
      const deshita = b4.aux({ lemma: 'です', text: 'でした' }, 'deshita');

      b4.inOrder(ka, no, 2);
      b4.inOrder(no, you, 2);
      b4.auxOf(you, deshita);

      b4.captureSpan('かのようだ', ka, deshita);
    },

    // Branch 5: Noun/Na-adj + である + か + の + ようだ
    // e.g., 名高い歌手であるかのようだ
    (b5) => {
      const ka = b5.particle('か', 'ka');
      const no = b5.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b5.aux({ lemma: 'よう' }, 'you');
      const da = b5.tok({ text: 'だ', lemma: 'だ' }, 'da');

      // Check for である before か
      const dearu = b5.tok({
        textOneOf: ['である', 'だ'],
        lemma: 'だ'
      }, 'dearu');

      b5.inOrder(dearu, ka, 3);
      b5.inOrder(ka, no, 2);
      b5.inOrder(no, you, 2);
      b5.auxOf(you, da);

      b5.captureSpan('かのようだ', ka, da);
    },

    // Branch 6: Verb/plain form + か + の + ように (adverbial)
    // e.g., 知っているかのように料理を作っている
    (b6) => {
      const ka = b6.particle('か', 'ka');
      const no = b6.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b6.tok({ lemma: 'よう' }, 'you');
      const ni = b6.particle('に', 'ni');

      b6.inOrder(ka, no, 2);
      b6.inOrder(no, you, 2);
      b6.inOrder(you, ni, 1);

      b6.captureSpan('かのようだ', ka, ni);
    },

    // Branch 7: Verb/plain form + か + の + ような (adnominal)
    // e.g., 母国に帰ってきたかのような感じ
    (b7) => {
      const ka = b7.particle('か', 'ka');
      const no = b7.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b7.tok({ lemma: 'よう' }, 'you');
      const na = b7.aux({
        lemma: 'だ',
        inflectionForm: '連体形-一般'
      }, 'na');

      b7.inOrder(ka, no, 2);
      b7.inOrder(no, you, 2);
      b7.inOrder(you, na, 1);

      b7.captureSpan('かのようだ', ka, na);
    },

    // Branch 8: Noun/Na-adj + である + か + の + ように (adverbial)
    // e.g., 自分の子供であるかのように可愛がってくれていた
    (b8) => {
      const ka = b8.particle('か', 'ka');
      const no = b8.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b8.tok({ lemma: 'よう' }, 'you');
      const ni = b8.particle('に', 'ni');

      // Check for である before か
      const dearu = b8.tok({
        textOneOf: ['である', 'だ'],
        lemma: 'だ'
      }, 'dearu');

      b8.inOrder(dearu, ka, 3);
      b8.inOrder(ka, no, 2);
      b8.inOrder(no, you, 2);
      b8.inOrder(you, ni, 1);

      b8.captureSpan('かのようだ', ka, ni);
    },

    // Branch 9: Noun/Na-adj + である + か + の + ような (adnominal)
    // e.g., あの有名な作品を真似たかのようなね
    (b9) => {
      const ka = b9.particle('か', 'ka');
      const no = b9.tok({ text: 'の', posOneOf: ['PART', 'SCONJ'] }, 'no');
      const you = b9.tok({ lemma: 'よう' }, 'you');
      const na = b9.aux({
        lemma: 'だ',
        inflectionForm: '連体形-一般'
      }, 'na');

      // Check for である before か
      const dearu = b9.tok({
        textOneOf: ['である', 'だ'],
        lemma: 'だ'
      }, 'dearu');

      b9.inOrder(dearu, ka, 3);
      b9.inOrder(ka, no, 2);
      b9.inOrder(no, you, 2);
      b9.inOrder(you, na, 1);

      b9.captureSpan('かのようだ', ka, na);
    },

    // Branch 10: Most flexible pattern - just require か + の + よう in order
    // For edge cases with different parse structures
    (b10) => {
      const ka = b10.tok({ text: 'か' }, 'ka');
      const no = b10.tok({ text: 'の' }, 'no');
      const you = b10.tok({ lemma: 'よう' }, 'you');

      b10.inOrder(ka, no, 2);
      b10.inOrder(no, you, 3);

      b10.captureSpan('かのようだ', ka, you);
    }
  );
});
