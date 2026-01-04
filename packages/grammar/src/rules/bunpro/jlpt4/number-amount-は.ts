import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: Number/Amount + は (at least, or so)
 *
 * Matches は following a number/counter or quantity expression to indicate
 * "at least" or "or so" with a contrastive nuance.
 *
 * Structures:
 * - Number + Counter + は (e.g., ５回は, ８回は, ３秒は, ６人は)
 * - Noun + くらい/ぐらい + は (e.g., クリスマスぐらいは, ２キロくらいは)
 * - Noun + は (when noun is a quantity concept like 肉, 手)
 *
 * Examples:
 * - ディズニーランドには年に５回は行っている (I go to Disneyland at least 5 times a year)
 * - 私は毎日、テレビを５時間は見ている (I watch TV for 5 hours or so every day)
 * - ２キロくらいはあると思う (I think it's at least 2 kilograms)
 * - クリスマスぐらいは家に帰って来てね (Come home for Christmas, at least)
 * - 囚人のうち少なくとも６人は逃げたらしい (At least 6 prisoners seem to have escaped)
 *
 * Key discriminators:
 * - は has dep=case and tag=助詞-係助詞
 * - Must follow a number+counter, noun+くらい/ぐらい, or quantity noun
 * - Different from topic は which marks sentence subject
 *
 * GiNZA parse structure:
 * - ５回は: ５(NUM, nummod→回) + 回(NOUN, nsubj) + は(ADP, case)
 * - ２キロくらいは: ２(NUM, nummod→キロ) + キロ(NOUN) + くらい(ADP, case) + は(ADP, case)
 * - クリスマスぐらいは: クリスマス(NOUN) + ぐらい(ADP, case) + は(ADP, case)
 * - 肉ぐらいは: 肉(NOUN) + ぐらい(ADP, case) + は(ADP, case)
 */
export default linguisticRule('number-amount-は', (r) => {
  r.either(
    // Branch 1: Number + Counter + は
    // Examples: ５回は, ８回は, ３秒は, ６人は, １回は
    // Pattern: NUM (nummod→NOUN) + NOUN (counter) + は (case)
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const counter = b.tok({ pos: 'NOUN' }, 'counter');
      const wa = b.particle('は', 'wa');
      b.headChild(counter, num, 'nummod');
      b.caseMarker(counter, wa);
      b.captureSpan('number-amount-は', num, wa);
    },

    // Branch 2: Number + Noun + くらい/ぐらい + は
    // Examples: ２キロくらいは, １回ぐらいは
    // Pattern: NUM + NOUN + くらい/ぐらい (ADP with lemma=くらい or ぐらい) + は
    (b) => {
      const num = b.tok({ pos: 'NUM' }, 'num');
      const noun = b.tok({ pos: 'NOUN' }, 'noun');
      const kurai = b.tok({
        pos: 'ADP',
        lemmaOneOf: ['くらい', 'ぐらい'],
      }, 'kurai');
      const wa = b.particle('は', 'wa');
      b.inOrder(num, noun, 1);
      b.inOrder(noun, kurai, 1);
      b.inOrder(kurai, wa, 1);
      b.captureSpan('number-amount-は', num, wa);
    },

    // Branch 3: Noun + くらい/ぐらい + は (without preceding number)
    // Examples: クリスマスぐらいは, 肉ぐらいは, 手ぐらいは
    // Pattern: NOUN + くらい/ぐらい + は
    (b) => {
      const noun = b.tok({ posOneOf: ['NOUN', 'PROPN'] }, 'noun');
      const kurai = b.tok({
        pos: 'ADP',
        lemmaOneOf: ['くらい', 'ぐらい'],
      }, 'kurai');
      const wa = b.particle('は', 'wa');
      b.caseMarker(noun, kurai);
      b.inOrder(kurai, wa, 1);
      b.captureSpan('number-amount-は', noun, wa);
    },

    // Branch 4: Just は after a quantity (when quantity is implicit from context)
    // Examples: ちょっとは, 少しは (after adverbs)
    // Pattern: ADV/ADJ + は
    (b) => {
      const adv = b.tok({ posOneOf: ['ADV', 'ADJ'] }, 'adv');
      const wa = b.particle('は', 'wa');
      b.inOrder(adv, wa, 2);
      b.captureSpan('number-amount-は', adv, wa);
    }
  );
});
