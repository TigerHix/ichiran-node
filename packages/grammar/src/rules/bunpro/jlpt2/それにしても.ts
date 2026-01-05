import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: それにしても (sorenishitemo) - "even so, nevertheless, having said that"
 *
 * A conjunction used to acknowledge a previous statement while adding a contrasting
 * or emphatic follow-up. It means "even so" or "nevertheless" - accepting the previous
 * context but emphasizing that the speaker's main point or reaction follows.
 *
 * Structures:
 * - [Statement A]. それにしても、[Statement B].
 * - [Statement A]。それにしても[Statement B].
 * - それにしても、[Statement B]. (sentence-initial)
 *
 * Examples:
 * - 彼女は新人だから仕方がないが、それにしても仕事ができなさすぎる。
 *   (She's new so we can't blame her, but even so, she is not good at her job at all.)
 * - 田中くんは遅れてくると言っていたが、それにしてもくるのが遅すぎる。
 *   (Tanaka-kun said he will be late, but even so, it's taking him too long to come.)
 * - 漢字を勉強し始めてから1年になる。それにしても、まだ全然漢字が読めない。
 *   (It has been a year since I started studying kanji. Nevertheless, I still can't read kanji at all.)
 *
 * Key characteristics:
 * - Acknowledges previous statement as fact/true
 * - Adds speaker's real thought, reaction, or stronger opinion
 * - Often expresses surprise, criticism, or emphasis
 * - Different from それでも - それにしても is more conversational and personal
 * - Different from それなのに - それにしても accepts previous context, それなのに is more contrastive
 *
 * GiNZA parse structure:
 * - Often parsed as single ADV or CCONJ token
 * - Sometimes split: それ (PRON) + に (ADP) + しても ( AUX/ADV)
 * - dep=advmod, dep=discourse, or dep=cc (conjunction usage)
 *
 * Different from similar conjunctions:
 * - それでも (soredemo) - "but still" (neutral contrast, less personal)
 * - それなのに (sorenanon) - "and yet" (stronger contradiction/contrast)
 * - ところが (tokoroga) - "however" (introduces unexpected result)
 * - にもかかわらず (nimokakawarazu) - "despite" (formal, strong contrast)
 */
export default linguisticRule('それにしても', (r) => {
  // それにしても is a conjunction meaning "even so, nevertheless"
  // It appears at the beginning of sentences or after punctuation
  // to acknowledge previous context while adding the speaker's main point
  //
  // GiNZA typically parses それにしても in one of two ways:
  // 1. Single token: ADV or CCONJ with dep=advmod/discourse/cc
  // 2. Split: それ (PRON) + に (ADP) + しても (ADV/AUX)
  //
  // The key discriminator is that it functions as a conjunction/discourse marker,
  // not as a pronoun + locative particle + verb phrase.

  r.either(
    // Pattern 1: Single token - most common parsing
    // それにしても as ADV or CCONJ (conjunction)
    (b1) => {
      const sorenishitemo = b1.tok({
        text: 'それにしても',
        posOneOf: ['ADV', 'CCONJ'],
        depOneOf: ['advmod', 'discourse', 'cc'],
      }, 'sorenishitemo');
      b1.capture(sorenishitemo);
    },

    // Pattern 2: Split into それ + に + しても
    // Less common but occurs in some parses
    (b2) => {
      const sore = b2.tok({
        text: 'それ',
        posOneOf: ['PRON', 'NOUN'],
      }, 'sore');
      const ni = b2.tok({
        text: 'に',
        posOneOf: ['ADP', 'PART'],
        depOneOf: ['dep', 'case', 'mark'],
      }, 'ni');
      const shitemo = b2.tok({
        textOneOf: ['しても', '為にして'],
        posOneOf: ['ADV', 'AUX', 'SCONJ'],
      }, 'shitemo');

      b2.inOrder(sore, ni, 1);
      b2.inOrder(ni, shitemo, 1);
      b2.captureSpan('それにしても', sore, shitemo);
    }
  );
});
