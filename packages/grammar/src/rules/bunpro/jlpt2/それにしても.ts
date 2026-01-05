import { bunproLinguisticRule } from '../../../engine/lang.js';

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
export default bunproLinguisticRule('それにしても', (r) => {
  // それにしても is a conjunction meaning "even so, nevertheless"
  // It appears at the beginning of sentences or after punctuation
  // to acknowledge previous context while adding the speaker's main point
  //
  // GiNZA parses それにしても as:
  // それ (PRON) + に (SCONJ) + し (SCONJ,lemma=する) + て (SCONJ) + も (SCONJ)
  // All with dep=fixed pointing to に (mark particle)
  //
  // The key discriminator is that:
  // - それ has dep=dep (conjunction/discourse usage) pointing to the root verb
  // - に has dep=mark (case marker)
  // - し,て,も all have dep=fixed (fixed expression marker)

  const sore = r.tok({
    text: 'それ',
    pos: 'PRON',
    depOneOf: ['dep', 'obl', 'cc'],
  }, 'sore');

  const ni = r.tok({
    text: 'に',
    pos: 'SCONJ',
    dep: 'mark',
  }, 'ni');

  const shi = r.tok({
    text: 'し',
    lemma: 'する',
    pos: 'SCONJ',
    dep: 'fixed',
  }, 'shi');

  const te = r.tok({
    text: 'て',
    pos: 'SCONJ',
    dep: 'fixed',
  }, 'te');

  const mo = r.tok({
    text: 'も',
    pos: 'SCONJ',
    dep: 'fixed',
  }, 'mo');

  r.inOrder(sore, ni, 1);
  r.inOrder(ni, shi, 1);
  r.inOrder(shi, te, 1);
  r.inOrder(te, mo, 1);

  r.captureSpan('それにしても', sore, mo);
});
