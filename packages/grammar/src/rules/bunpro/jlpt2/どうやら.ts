import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: どうやら (douyara) - "apparently, it seems like, looks like"
 *
 * An adverb expressing conjecture or inference based on observation or evidence.
 * Indicates uncertainty or confusion about how something appears to be.
 * Typically used at the beginning of sentences or before clauses.
 *
 * Structures:
 * - どうやら + conjectural ending (みたいだ, ようだ, らしい, そうだ)
 * - どうやら + sentence expressing speculation
 *
 * Examples:
 * - どうやら彼は結婚しているらしいよ。
 *   (Apparently, he is married.)
 * - どうやら今日も休んでいるみたいだね。
 *   (It seems like he's taking today off as well.)
 * - どうやら私には必要がないようだ。
 *   (It seems like I am not needed.)
 *
 * Key discriminators:
 * - どうやら is an adverb expressing conjecture
 * - Used with conjectural expressions (みたい, ようだ, らしい, そうだ)
 * - Different from similar adverbs:
 *   - どうも (dōmo) - "thank you" or "by all means/quite"
 *   - なんとなく (nantonaku) - "somehow, vaguely"
 *   - どうも...ようだ (dōmo...yō da) - "it seems that" (but pattern is different)
 *   - なにやら (naniyara) - "something or other" (noun phrase)
 *
 * GiNZA parse structure:
 * - どうやら - may be parsed as single ADV token or multiple tokens
 * - The particle やら can also be used as a listing particle
 * - We match the exact text to avoid false positives
 */
export default linguisticRule('どうやら', (r) => {
  r.either(
    // Single token variant: どうやら as ADV
    (b) => {
      const douyara = b.tok({
        text: 'どうやら',
      }, 'douyara');
      b.capture(douyara);
    },

    // Multi-token variant: どう (ADV) + やら (ADP/PART)
    // Similar to どうしても which is parsed as 4 tokens
    (b) => {
      const dou = b.tok({ text: 'どう' }, 'dou');
      const yara = b.tok({
        text: 'やら',
        posOneOf: ['ADP', 'PART'],
      }, 'yara');

      b.inOrder(dou, yara, 1);
      b.captureSpan('どうやら', dou, yara);
    }
  );
});
