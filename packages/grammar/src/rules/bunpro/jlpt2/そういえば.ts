import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: そういえば (souieba) - "come to think of it, speaking of which, by the way"
 *
 * A discourse marker used to introduce a new topic or thought that was triggered
 * by the current conversation. It consists of adverb "そう" (like that/so) + verb
 * "言う" (to say) in conditional form "いえば" (if saying). Literally means
 * "if saying that" or "come to think of it".
 *
 * Structures:
 * - そういえば、[Statement].
 * - [Statement A]。そういえば、[Statement B].
 *
 * Examples:
 * - そういえば、この前借りた本読み終わったよ！
 *   (Come to think of it, I finished reading the book I borrowed from you the other day!)
 * - そういえば、昨日の卒業式はどうだった？
 *   (Speaking of which, how was the graduation ceremony yesterday?)
 * - そういえば、今日は母の日だ。
 *   (Come to think of it, it is Mother's Day today.)
 * - そういえば、大家さんに家賃振り込んでくれた？
 *   (Speaking of which, did you transfer the rent to the landlord for me?)
 *
 * Key characteristics:
 * - Appears at the beginning of sentences or clauses
 * - Introduces a topic or question triggered by previous conversation
 * - Similar to English "come to think of it" or "speaking of which"
 * - Different from ところで (by the way - unrelated topic change)
 * - Different from といえば (speaking of X - requires noun topic)
 *
 * GiNZA parse structure:
 * - そう (ADV) + いう (VERB) conditional form + ば (PART)
 * - Often parsed as compound or fixed expression
 * - ADV (そう) + AUX/VERB (いえば) with various dependencies
 *
 * Kanji variants:
 * - そういえば (standard hiragana - most common)
 * - 然言えば (kanji variant - rare)
 *
 * Different from similar expressions:
 * - といえば (to ieba) - "speaking of X" (requires noun topic)
 * - ところで (tokorode) - "by the way" (unrelated topic change)
 * - ちなみに (chinami ni) - "by the way, incidentally" (related information)
 * - そうすると (sousuruto) - "then, in that case" (consequence)
 */
export default linguisticRule('そういえば', (r) => {
  // そういえば is a fixed expression consisting of:
  // 1. そう (ADV - adverb "so/like that")
  // 2. いう (VERB - "to say" in conditional form "いえば")
  // 3. ば (PART/SCONJ - conditional particle)
  //
  // GiNZA typically parses this as:
  // - Compound/fixed expression with dep=compound, dep=fixed, or dep=discourse
  // - そう (ADV) + いう (VERB/AUX) + ば (PART/SCONJ)
  // - Sometimes as multi-token expression, sometimes as single token
  //
  // The expression appears:
  // 1. At sentence beginning (most common)
  // 2. After sentence boundary (period)
  // 3. As discourse marker with dep=discourse

  r.either(
    // Pattern 1: Multi-token parsing - そう(ADV) + いう(VERB/AUX) + ば(PART/SCONJ)
    // Most common GiNZA parsing as separate tokens with compound/fixed dependencies
    (b1) => {
      const sou = b1.adv({ text: 'そう' }, 'sou');
      const iu = b1.tok({
        lemmaOneOf: ['言う', '云う', '謂う'],
        inflectionForm: '仮定形-一般',
        posOneOf: ['VERB', 'AUX']
      }, 'iu');
      const ba = b1.particle('ば', 'ba', { posOneOf: ['PART', 'SCONJ'] });

      b1.inOrder(sou, iu, 3);
      b1.inOrder(iu, ba, 1);
      b1.headChild(sou, iu, 'compound');
      b1.headChild(sou, ba, 'compound');

      b1.captureSpan('そういえば', sou, ba);
    },

    // Pattern 2: Multi-token with fixed dependency
    (b2) => {
      const sou = b2.adv({ text: 'そう' }, 'sou');
      const iu = b2.tok({
        lemmaOneOf: ['言う', '云う', '謂う'],
        inflectionForm: '仮定形-一般',
        posOneOf: ['VERB', 'AUX']
      }, 'iu');
      const ba = b2.particle('ば', 'ba', { posOneOf: ['PART', 'SCONJ'] });

      b2.inOrder(sou, iu, 3);
      b2.inOrder(iu, ba, 1);
      b2.headChild(sou, iu, 'fixed');
      b2.headChild(sou, ba, 'fixed');

      b2.captureSpan('そういえば', sou, ba);
    },

    // Pattern 3: Multi-token with discourse dependency
    (b3) => {
      const sou = b3.adv({ text: 'そう' }, 'sou');
      const iu = b3.tok({
        lemmaOneOf: ['言う', '云う', '謂う'],
        inflectionForm: '仮定形-一般',
        posOneOf: ['VERB', 'AUX']
      }, 'iu');
      const ba = b3.particle('ば', 'ba', { posOneOf: ['PART', 'SCONJ'] });

      b3.inOrder(sou, iu, 3);
      b3.inOrder(iu, ba, 1);
      b3.headChild(sou, iu, 'discourse');
      b3.headChild(sou, ba, 'discourse');

      b3.captureSpan('そういえば', sou, ba);
    },

    // Pattern 4: Catch-all multi-token (no specific dependency required)
    // For unusual GiNZA parsings
    (b4) => {
      const sou = b4.adv({ text: 'そう' }, 'sou');
      const iu = b4.tok({
        lemmaOneOf: ['言う', '云う', '謂う'],
        inflectionForm: '仮定形-一般'
      }, 'iu');
      const ba = b4.particle('ば', 'ba');

      b4.inOrder(sou, iu, 3);
      b4.inOrder(iu, ba, 1);

      b4.captureSpan('そういえば', sou, ba);
    },

    // Pattern 5: Single token parsing (if GiNZA treats it as one token)
    // Some tokenizers may treat the entire expression as a single adverb
    (b5) => {
      const souieba = b5.tok({
        textOneOf: ['そういえば', '然言えば'],
        posOneOf: ['ADV', 'INTJ', 'SCONJ']
      }, 'souieba');

      b5.capture(souieba);
    }
  );
});
