import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: というものでもない (to iu mono demo nai) - Not necessarily the case
 *
 * A grammar pattern meaning "not necessarily" or "not always the case".
 * It expresses that while something might appear to be true or is often
 * thought to be true, it's not an absolute rule. This is a formal way
 * to say "there's no guarantee that..." or "it's not necessarily true that...".
 *
 * Structure:
 * - Verb + というものでもない
 * - I-adjective + というものでもない
 * - Na-adjective + (だ) + というものでもない
 * - Noun + (だ) + というものでもない
 *
 * The negative can also be:
 * - というものではない (slightly less formal)
 * - というものではありません (polite)
 * - というものでもありません (polite)
 * - というものじゃない (casual)
 *
 * Examples:
 * - 図鑑にはすべての生き物が載っているというものでもない。
 *   (It's not necessarily true that all living creatures are listed in the encyclopedia.)
 * - 洋服は高ければいいというものでもない。
 *   (It's not necessarily true that clothes are better if they're expensive.)
 * - いい大学を出たからといって、いい会社に入社できるというものでもない。
 *   (Just because you graduated from a good university doesn't necessarily mean you can get into a good company.)
 *
 * Key discriminators:
 * - The pattern という + もの + でも + ない must appear together
 * - という is the quotative particle combination
 * - もの is a noun meaning "thing/abstract concept"
 * - でも is a particle meaning "even/not even"
 * - ない (or ありません/じゃない) is the negation
 * - The entire pattern attaches to the preceding predicate
 *
 * Different from:
 * - というわけではない (different nuance - counters an assumption)
 * - わけではない (direct denial of assumption)
 * - とは限らない (not necessarily - less formal)
 * - ことにはならない (doesn't mean that - weaker)
 *
 * GiNZA parse structure:
 * - という as separate tokens (quotative particle combination)
 * - もの as NOUN
 * - でも as ADP or PART
 * - ない/ありません as AUX or ADJ
 */
export default linguisticRule('というものでもない', (r) => {
  r.either(
    // Pattern 1: Full combined token - entire pattern as one unit
    (b1) => {
      const pattern = b1.tok({
        textOneOf: [
          'というものでもない',
          'というものでもない。',
          'というものではない',
          'というものではない。',
          'というものじゃない',
          'というものじゃない。'
        ]
      }, 'pattern');

      b1.capture(pattern);
    },

    // Pattern 2: という + もの + でも + ない (most common tokenization)
    (b2) => {
      const toiu = b2.tok({ text: 'という' }, 'toiu');
      const mono = b2.tok({ text: 'もの' }, 'mono');
      const demo = b2.tok({ text: 'でも' }, 'demo');
      const nai = b2.tok({ text: 'ない' }, 'nai');

      b2.inOrder(toiu, mono, 10);
      b2.inOrder(mono, demo, 10);
      b2.inOrder(demo, nai, 10);

      b2.captureSpan('というものでもない', toiu, nai);
    },

    // Pattern 3: と + いう + もの + でも + ない (split と/いう)
    (b3) => {
      const to = b3.tok({ text: 'と' }, 'to');
      const iu = b3.tok({ text: 'いう' }, 'iu');
      const mono = b3.tok({ text: 'もの' }, 'mono');
      const demo = b3.tok({ text: 'でも' }, 'demo');
      const nai = b3.tok({ text: 'ない' }, 'nai');

      b3.inOrder(to, iu, 1);
      b3.inOrder(iu, mono, 10);
      b3.inOrder(mono, demo, 10);
      b3.inOrder(demo, nai, 10);

      b3.captureSpan('というものでもない', to, nai);
    }
  );
});
