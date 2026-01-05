import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: それなら (sore nara) - "in that case, if so, if that's the case"
 *
 * A discourse marker/conjunction used to respond to a previous statement by
 * presenting a consequence, suggestion, or course of action based on that statement.
 * It functions as a conditional conjunction meaning "if that's the case, then..."
 *
 * Structures:
 * - [Statement A]. それなら、[Statement B].
 * - それなら、[Statement B]. (at beginning of sentence)
 * - [Statement A]? それなら、[Statement B].
 *
 * Examples:
 * - 駅の近くにあるスーパーに行くの？それならついでに私を駅まで送ってくれない？
 *   (You're going to the supermarket near the station? If that's the case, can you drop me off at the station?)
 * - カメラの画質がいいスマホをお探しなんですか？それならこのスマホをお勧めします。
 *   (You are looking for a smartphone with a good camera? If that's the case, I recommend this one.)
 * - それなら彼でもできるようだ。
 *   (If that's the case, it seems like even he will be able to do it.)
 * - それなら早く警察に電話したほうがいいよ！
 *   (If that's the case, you should call the cops as soon as possible.)
 *
 * Key characteristics:
 * - Responds to previous context or statement
 * - Expresses conditional consequence: "if X is true, then Y"
 * - Often introduces suggestions, advice, or logical conclusions
 * - Can appear at sentence beginning or after punctuation
 * - Conversational and response-oriented
 *
 * Variants:
 * - それなら (standard form)
 * - だったら (casual contraction of "それだったら")
 * - それだったら (emphatic variant)
 * - そうだったら (referring to situation/state)
 *
 * GiNZA parse structure:
 * - それ (pronoun/demonstrative) + なら (particle/conjunction)
 * - Or parsed as single conjunction token
 * - dep=dep or dep=discourse (conjunction/discourse marker usage)
 *
 * Different from similar markers:
 * - では/それでは (dewa/soredewa) - broader: "well then", "in that case", "with that"
 * - その場合なら (sono baai nara) - more formal: "in that specific case"
 * - そうすれば (sureba) - conditional: "if (you) do that"
 * - なら (nara) alone - general conditional particle without "that" reference
 */
export default linguisticRule('それなら', (r) => {
  // それなら is a discourse marker that responds to previous context
  // It can appear in several forms:
  // 1. それなら (standard: "that" + conditional particle)
  // 2. だったら (casual: contraction of "sore dattara")
  // 3. それだったら (emphatic: "that" + "was" + conditional)
  //
  // GiNZA typically parses それなら as:
  // - それ (PRON/CCONJ) + なら (ADP/SCONJ/PART)
  // - Sometimes as single token
  // - なら typically has dep=dep or dep=discourse for conjunction usage
  //
  // The key is that なら functions as a conditional conjunction here,
  // not as a topic marker or locative particle.

  r.either(
    // Pattern 1: それなら - それ (demonstrative) + なら (conditional conjunction)
    (b) => {
      const sore = b.tok({
        text: 'それ',
      }, 'sore');
      const nara = b.tok({
        text: 'なら',
      }, 'nara');
      b.inOrder(sore, nara, 1);
      b.captureSpan('それなら', sore, nara);
    },
    // Pattern 2: だったら - casual conversational form
    (b) => {
      const dattara = b.tok({
        text: 'だったら',
      }, 'dattara');
      b.capture(dattara);
    },
    // Pattern 3: それだったら - emphatic variant
    (b) => {
      const sore = b.tok({
        text: 'それ',
      }, 'sore');
      const dattara = b.tok({
        text: 'だったら',
      }, 'dattara');
      b.inOrder(sore, dattara, 1);
      b.captureSpan('それだったら', sore, dattara);
    }
  );
});
