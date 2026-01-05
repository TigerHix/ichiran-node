import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: たった（の）(tatta no) - "only, just, no more than"
 *
 * An emphatic adverb indicating that something is merely or no more than
 * a certain amount. It's a stronger, more emphasized version of "ただ" (tada).
 *
 * Structures:
 * - たった + Number/Amount
 * - たったの + Number/Amount
 *
 * Examples:
 * - カップ麺はたった３分で出来上がる。
 *   (Cup noodles can be prepared in just 3 minutes.)
 * - クラスメイト全員を招待したのに、たったの３人しか僕の誕生日パーティーにこなかった。
 *   (I invited everyone in my class, but only 3 people showed up to my birthday party.)
 * - 彼はたった一人の兄弟です。
 *   (He is my only sibling.)
 *
 * Key discriminators:
 * - Written as たった (hiragana) - the kanji form 只っ is rarely used
 * - Functions as an adverb (ADV) emphasizing smallness of amount
 * - May be followed by particle の (optional) when modifying a number/noun
 * - Always precedes a quantity, number, or amount being emphasized
 * - Different from 立つ (tatsu - to stand), 尋ねる (tazuneru - to ask)
 *
 * GiNZA parse structure:
 * - たった (ADV) - adverb
 * - May be followed by:
 *   - の (ADP/PART) - particle when modifying noun phrases
 *   - Direct number (NUM) or noun phrase
 *
 * Different from:
 * - ただ (tada) - "only/merely" (less emphatic)
 * - わずか (wazuka) - "slightly/barely" (more formal)
 * - 仅仅 (merry) - Chinese loan word with similar meaning
 */
export default bunproLinguisticRule('たった-の', (r) => {
  r.either(
    // Pattern 1: たった + の + Number/Noun (with particle)
    // Most common pattern when emphasizing a specific amount
    (b1) => {
      const tatta = b1.tok({
        lemma: 'たった',
        pos: 'ADV',
      }, 'tatta');
      const no = b1.particle('の', 'no');

      b1.inOrder(tatta, no, 1);
      b1.captureSpan('たったの', tatta, no);
    },

    // Pattern 2: たった alone (without particle)
    // Often used at sentence start or before numbers
    (b2) => {
      const tatta = b2.tok({
        lemma: 'たった',
        pos: 'ADV',
      }, 'tatta');

      // Capture just the adverb
      b2.capture(tatta);
    }
  );
});
