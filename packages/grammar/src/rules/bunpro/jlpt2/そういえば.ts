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
 * - そう (ADV) + いえ (VERB, conditional stem) + ば (AUX/PART)
 * - Often parsed as compound or fixed expression
 * - ADV (そう) + VERB (いえ) + AUX (ば)
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
  // 2. いえ (VERB - conditional stem of "言う")
  // 3. ば (AUX/PART/SCONJ - conditional particle)
  //
  // GiNZA typically parses this as 3 tokens:
  // - そう (ADV)
  // - いえ (VERB, lemma=言う, inflectionForm=仮定形-一般)
  // - ば (AUX or PART or SCONJ)
  //
  // The expression appears:
  // 1. At sentence beginning (most common)
  // 2. After sentence boundary (period)
  // 3. As discourse marker with dep=discourse

  r.either(
    // Pattern 1: そう(ADV) + いえ(VERB) + ば(AUX) with compound dependency
    (b1) => {
      const sou = b1.adv({ text: 'そう' }, 'sou');
      const ie = b1.verb({ text: 'いえ' }, 'ie');
      const ba = b1.tok({ text: 'ば', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'ba');

      b1.inOrder(sou, ie, 1);
      b1.inOrder(ie, ba, 1);
      b1.headChild(sou, ie, 'compound');
      b1.headChild(sou, ba, 'compound');

      b1.captureSpan('そういえば', sou, ba);
    },

    // Pattern 2: そう(ADV) + いえ(VERB) + ば with fixed dependency
    (b2) => {
      const sou = b2.adv({ text: 'そう' }, 'sou');
      const ie = b2.verb({ text: 'いえ' }, 'ie');
      const ba = b2.tok({ text: 'ば', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'ba');

      b2.inOrder(sou, ie, 1);
      b2.inOrder(ie, ba, 1);
      b2.headChild(sou, ie, 'fixed');
      b2.headChild(sou, ba, 'fixed');

      b2.captureSpan('そういえば', sou, ba);
    },

    // Pattern 3: そう(ADV) + いえ(VERB) + ば with discourse dependency
    (b3) => {
      const sou = b3.adv({ text: 'そう' }, 'sou');
      const ie = b3.verb({ text: 'いえ' }, 'ie');
      const ba = b3.tok({ text: 'ば', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'ba');

      b3.inOrder(sou, ie, 1);
      b3.inOrder(ie, ba, 1);
      b3.headChild(sou, ie, 'discourse');
      b3.headChild(sou, ba, 'discourse');

      b3.captureSpan('そういえば', sou, ba);
    },

    // Pattern 4: Catch-all pattern - just match the sequence without specific dependencies
    // For unusual GiNZA parsings or edge cases
    (b4) => {
      const sou = b4.adv({ text: 'そう' }, 'sou');
      const ie = b4.verb({ text: 'いえ' }, 'ie');
      const ba = b4.tok({ text: 'ば' }, 'ba');

      b4.inOrder(sou, ie, 1);
      b4.inOrder(ie, ba, 1);

      b4.captureSpan('そういえば', sou, ba);
    },

    // Pattern 5: Single token parsing (if GiNZA treats it as one token)
    // Some tokenizers may treat the entire expression as a single token
    (b5) => {
      const souieba = b5.tok({
        textOneOf: ['そういえば', '然言えば'],
        posOneOf: ['ADV', 'INTJ', 'SCONJ']
      }, 'souieba');

      b5.capture(souieba);
    },

    // Pattern 6: Lemma-based matching for "言う" in conditional form
    // In case GiNZA uses different surface forms
    (b6) => {
      const sou = b6.adv({ text: 'そう' }, 'sou');
      const iu = b6.tok({
        lemmaOneOf: ['言う', '云う', '謂う'],
        posOneOf: ['VERB', 'AUX']
      }, 'iu');
      const ba = b6.tok({ text: 'ば', posOneOf: ['AUX', 'PART', 'SCONJ'] }, 'ba');

      b6.inOrder(sou, iu, 3);
      b6.inOrder(iu, ba, 1);

      b6.captureSpan('そういえば', sou, ba);
    }
  );
});
