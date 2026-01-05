import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: げ (ge) - "appearance, sign, look of"
 *
 * A suffix that attaches to adjective stems and verb stems to create
 * new な-adjectives meaning "seemingly" or "giving the appearance of".
 * Often written in kanji as 気（げ）. More subjective than そう.
 *
 * Formation patterns:
 * - I-adjective stem + げ: 悲しげな (sad-looking), 楽しげに (enjoyably-seeming)
 * - Na-adjective stem + げ: 満足げな (satisfied-looking)
 * - Verb stem + げ: (rare in test data, but grammatically possible)
 *
 * The resulting form is a な-adjective:
 * - げな + Noun: 悲しげな顔 (sad-looking face)
 * - げに + Verb: 楽しげに遊ぶ (seemingly enjoying playing)
 *
 * Examples from test data:
 * - 悲しげな顔: sad-looking face
 * - くやしげな表情: frustrated-looking expression
 * - たのしげに: seemingly enjoying
 * - なつかしげに: nostalgically-seeming
 * - はずかしげに: seemingly embarrassed
 * - まんぞくげな: satisfied-looking
 * - かなしげな: sad-looking
 * - ありげな: meaningful-looking (意味ありげな)
 * - すずしげな: cool/unruffled-looking
 * - うらやましげに: enviously-seeming
 * - あやしげな: suspicious-looking
 * - はかなげな: fragile/fickle-looking
 * - かわいげ: cute-seemingness (noun form, rare)
 *
 * GiNZA parse structure:
 * - げ is parsed as PART with tag=接尾辞-形容詞的 (adjectival suffix)
 * - The base word (adjective stem) precedes it
 * - May be followed by:
 *   - な (ADP) - adnominal particle before noun
 *   - に (ADP) - adverbial particle before verb
 *
 * Key discriminators:
 * - More subjective and lower confidence than そう
 * - Different from み (creates abstract nouns)
 * - Different from っぽい (innate traits vs perceived emotions)
 *
 * Strategy: Match げ as a suffix followed by な or に.
 * Standalone げ (without な/に) is rare but exists (かわいげ).
 */
export default linguisticRule('げ', (r) => {
  r.either(
    // Pattern 1: げ + な + Noun (most common)
    // Creates な-adjective modifying a noun
    (b1) => {
      const ge = b1.tok({
        textOneOf: ['げ', '気'],
        tag: '接尾辞-形容詞的',
      }, 'ge');
      const na = b1.particle('な', 'na');
      const noun = b1.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');

      b1.inOrder(ge, na, 1);
      b1.inOrder(na, noun, 1);
      b1.captureSpan('げ', ge, noun);
    },

    // Pattern 2: げ + に + Verb/Phrase
    // Adverbial use modifying a verb
    (b2) => {
      const ge = b2.tok({
        textOneOf: ['げ', '気'],
        tag: '接尾辞-形容詞的',
      }, 'ge');
      const ni = b2.particle('に', 'ni');

      b2.inOrder(ge, ni, 1);
      b2.captureSpan('げ', ge, ni);
    },

    // Pattern 3: Standalone げ (without particle)
    // Rare cases like かわいげ (as a noun)
    (b3) => {
      const ge = b3.tok({
        textOneOf: ['げ', '気'],
        tag: '接尾辞-形容詞的',
      }, 'ge');

      b3.capture(ge);
    }
  );
});
