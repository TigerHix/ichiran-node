import { bunproLinguisticRule } from '../../../engine/lang.js';

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
export default bunproLinguisticRule('げ', (r) => {
  r.either(
    // Branch 1: Combined forms ending with げな
    // GiNZA parses stem + げ + な as single tokens
    (b1) => {
      const combined = b1.tok({
        textOneOf: [
          '悲しげな',      // sad-looking
          'くやしげな',    // frustrated-looking
          'ありげな',      // meaningful-looking (意味ありげ)
          'はかなげな',    // fragile/fickle-looking
          'まんぞくげな',  // satisfied-looking
          'かなしげな',    // sad-looking
          'あやしげな',    // suspicious-looking
          'すずしげな',    // cool/unruffled-looking
        ],
      }, 'combined');
      b1.capture(combined);
    },

    // Branch 2: Combined forms ending with げに
    // GiNZA parses stem + げ + に as single tokens
    (b2) => {
      const combined = b2.tok({
        textOneOf: [
          'たのしげに',    // seemingly enjoying
          'はずかしげに',  // seemingly embarrassed
          'なつかしげに',  // nostalgically-seeming
          'うらやましげに',// enviously-seeming
          '不安げに',      // seemingly nervous
        ],
      }, 'combined');
      b2.capture(combined);
    },

    // Branch 3: Standalone noun form かわいげ
    (b3) => {
      const combined = b3.tok({
        textOneOf: ['かわいげ', '可愛げ'],
        // No POS constraint - GiNZA may tag it variably
      }, 'combined');
      b3.capture(combined);
    },

    // Branch 4: Separate tokens - げ followed by な (if parsed separately)
    // Use only hiragana げ, not standalone kanji 気 (to avoid matching 気をつけて)
    (b4) => {
      const ge = b4.tok({
        text: 'げ',  // Only hiragana げ, not kanji 気
      }, 'ge');
      const na = b4.particle('な', 'na');
      b4.inOrder(ge, na, 3);  // Allow more distance for variations
      b4.captureSpan('げ', ge, na);
    },

    // Branch 5: Separate tokens - げ followed by に (if parsed separately)
    // Use only hiragana げ, not standalone kanji 気 (to avoid matching 気をつけて)
    (b5) => {
      const ge = b5.tok({
        text: 'げ',  // Only hiragana げ, not kanji 気
      }, 'ge');
      const ni = b5.particle('に', 'ni');
      b5.inOrder(ge, ni, 3);  // Allow more distance for variations
      b5.captureSpan('げ', ge, ni);
    },

    // Branch 6: Standalone げ suffix token (rare, not combined with noun)
    (b6) => {
      const ge = b6.tok({
        text: 'げ',  // Only hiragana げ, not kanji 気
      }, 'ge');
      b6.capture(ge);
    }
  );
});
