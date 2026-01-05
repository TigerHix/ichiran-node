import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: なくはない (naku wa nai) - It's not that..., not impossible, somewhat
 *
 * A double negative expression meaning "it's not that (A) doesn't exist/happen" or
 * "(A) is not impossible." Indicates partial affirmation or hesitant agreement.
 *
 * Structures:
 * - Verb［negative stem］+ は + ない (e.g., できなくはない, 走れなくはない)
 * - I-adj［く］+ は + ない (e.g., わからくはない)
 * - Noun + が/は/に + く + は + ない (e.g., がなくはない, はなくはない)
 * - Standalone なく + は + ない (e.g., 時間はなくはない)
 *
 * The pattern can also use も instead of は for emphasis (なくもない).
 *
 * Examples:
 * - できなくはないが、時間がかかる。
 *   (It's not that I can't do it, but it takes time.)
 * - わからなくはないが、説明しにくい。
 *   (It's not that I don't understand, but it's hard to explain.)
 * - 時間はなくはない。
 *   (It's not that I don't have time.)
 *
 * Key discriminators:
 * - なく is the adverbial form (ku-form) of ない
 * - Must be followed by topic marker は (or emphatic も)
 * - Must end with auxiliary ない
 * - Different from simple negation ない
 * - Different from くない (i-adjective negative)
 *
 * GiNZA parse structure:
 * - Often tokenizes as single token: "なくはない" or "なくもない"
 * - Sometimes splits as: "なくは" + "ない" or "なく" + "はない"
 * - Sometimes three tokens: "なく" + "は" + "ない"
 * - Sometimes parses なく + [auxiliaries] + ない with は merged into other tokens
 * - Various dependency relations (aux, fixed, compound)
 */
export default linguisticRule('なくはない', (r) => {
  r.either(
    // Branch 1: Single token "なくはない" or "なくもない"
    // This is the most common tokenization for GiNZA
    (b) => {
      const nakuhanai = b.tok({
        textOneOf: ['なくはない', 'なくもない'],
      }, 'nakuhanai');
      b.capture(nakuhanai);
    },

    // Branch 1b: Verb stem + なくはない or なくもない (single token)
    // Some tokenizations might include the verb stem + く in the same token
    // Also covers standalone "なく" cases (捜せばなくはない)
    (b) => {
      const nakuhanai = b.tok({
        textOneOf: [
          // Verb + なくはない/なくもない
          'できなくはない', 'できなくもない',
          'わからなくはない', 'わからなくもない',
          '食べれなくはない', '食べれなくもない',
          '走れなくはない', '走れなくもない',
          '見えなくはない', '見えなくもない',
          'けずれなくはない', 'けずれなくもない',
          'かえなくはない', 'かえなくもない',
          'くみたてられなくはない', 'くみたてられなくもない',
          // Standalone "なく" cases
          '捜せばなくはない', '捜せばなくもない',
          'なくはない', 'なくもない',
        ],
      }, 'nakuhanai');
      b.capture(nakuhanai);
    },

    // Branch 2: Split as "なくは" or "なくも" + "ない"
    (b) => {
      const nakuwa = b.tok({
        textOneOf: ['なくは', 'なくも'],
        lemma: 'ない',
      }, 'nakuwa');
      const nai = b.tok({
        text: 'ない',
        lemma: 'ない',
        posOneOf: ['AUX', 'VERB'],
      }, 'nai');
      b.inOrder(nakuwa, nai, 15);  // Large distance to handle intermediate tokens
      b.captureSpan('なくはない', nakuwa, nai);
    },

    // Branch 3: "なく" (lemma=ない) + "はない" with larger distance
    // Use exact text match to avoid partial matches like 行けない
    (b) => {
      const naku = b.tok({
        text: 'なく',
        lemma: 'ない',
      }, 'naku');
      const wa = b.tok({
        text: 'はない',  // Exact match only
      }, 'wa');
      b.inOrder(naku, wa, 20);  // Very large distance
      b.captureSpan('なくはない', naku, wa);
    },

    // Branch 4: "なく" + "は"/"も" + "ない" - the は might be a separate ADP particle
    (b) => {
      const naku = b.tok({
        text: 'なく',
        lemma: 'ない',
      }, 'naku');
      const wa = b.tok({
        textOneOf: ['は', 'も'],
        pos: 'ADP',
      }, 'wa');
      const nai = b.tok({
        text: 'ない',
        lemma: 'ない',
        posOneOf: ['AUX', 'VERB'],
      }, 'nai');
      b.inOrder(naku, wa, 10);
      b.inOrder(wa, nai, 10);
      b.captureSpan('なくはない', naku, nai);
    },

    // Branch 5: "わからなく" + "は" + "ない" pattern
    // Specifically for cases where GiNZA tokenizes verb+く as one token
    (b) => {
      const naku = b.tok({
        textOneOf: ['わからなく', 'できなく', '食べれなく', '走れなく', '見えなく', 'けずれなく', 'かえなく'],
      }, 'naku');
      const wa = b.tok({
        textOneOf: ['は', 'も'],
        pos: 'ADP',
      }, 'wa');
      const nai = b.tok({
        text: 'ない',
        lemma: 'ない',
        posOneOf: ['AUX', 'VERB'],
      }, 'nai');
      b.inOrder(naku, wa, 3);
      b.inOrder(wa, nai, 3);
      b.captureSpan('なくはない', naku, nai);
    }
  );
});
