import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: そういえば - Speaking of which, By the way, Come to think of it
 *
 * Matches そういえば, a discourse marker used to introduce a new topic
 * that was triggered by the current conversation. Literally means "if saying so"
 * (そう + いう + ば).
 *
 * Structure:
 * - そういえば (discourse marker at sentence beginning)
 *
 * Examples:
 * - そういえば、昨日彼に会った。
 *   (Come to think of it, I saw him yesterday.)
 * - そういえば、今日は母の日だ。
 *   (Speaking of which, today is Mother's Day.)
 * - そういえば、この前借りた本読み終わったよ！
 *   (By the way, I finished reading the book I borrowed from you the other day!)
 *
 * Key discriminators:
 * - POS is ADV or CCONJ (can be parsed as either)
 * - Typically appears at the beginning of a sentence
 * - This is a fixed expression/set phrase
 *
 * GiNZA parse structure:
 * - May be tokenized as single token: そういえば
 * - May be tokenized as two tokens: そう + いえば
 * - May be tokenized as three tokens: そう + いえ + ば (いえ is the stem form of いう)
 * - May be tokenized as three tokens: そう + いう + ば
 *
 * Usage notes:
 * - Used when the speaker remembers something related to the current topic
 * - Different from ところで (complete topic change) or といえば (topic introduction after a noun)
 * - Requires some contextual trigger from the conversation
 */
export default linguisticRule('そういえば', (r) => {
  r.either(
    // Pattern 1: Single token そういえば
    (b) => {
      const souieba = b.tok({ text: 'そういえば' }, 'souieba');
      b.capture(souieba);
    },
    // Pattern 2: Two tokens - そう + いえば
    (b) => {
      const sou = b.tok({ text: 'そう' }, 'sou');
      const ieba = b.tok({ text: 'いえば' }, 'ieba');
      b.inOrder(sou, ieba, 1);
      b.captureSpan('そういえば', sou, ieba);
    },
    // Pattern 3: Three tokens - そう + いえ + ば (いえ is stem form of いう)
    (b) => {
      const sou = b.tok({ text: 'そう' }, 'sou');
      const ie = b.tok({ text: 'いえ' }, 'ie');
      const ba = b.tok({ text: 'ば' }, 'ba');
      b.inOrder(sou, ie, 1);
      b.inOrder(ie, ba, 1);
      b.captureSpan('そういえば', sou, ba);
    },
    // Pattern 4: Three tokens - そう + いう + ば
    (b) => {
      const sou = b.tok({ text: 'そう' }, 'sou');
      const iu = b.tok({ text: 'いう' }, 'iu');
      const ba = b.tok({ text: 'ば' }, 'ba');
      b.inOrder(sou, iu, 1);
      b.inOrder(iu, ba, 1);
      b.captureSpan('そういえば', sou, ba);
    }
  );
});
