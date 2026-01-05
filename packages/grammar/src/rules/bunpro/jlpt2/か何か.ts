import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: か何か (ka nanika) - Or something, Or something like that
 *
 * A casual phrase expressing uncertainty about a specific thing.
 * Indicates "X or something like that", "X or whatnot".
 * The speaker is unsure about the exact thing but gives an example.
 *
 * Structure: Noun + か + 何か/なにか
 *
 * Can be written as:
 * - か何か (mixed kanji/kana - formal/written)
 * - かなにか (all hiragana - casual/spoken)
 *
 * Examples:
 * - お茶かなにかありませんか？
 *   (Do you have tea or something?)
 * - ビールかなにかがほしいな。
 *   (I'd like a beer or something.)
 * - コーヒーかなにか飲みませんか？
 *   (Would you like coffee or something to drink?)
 * - 交通渋滞かなにかのような気がする。
 *   (I have a feeling that he is stuck in traffic or something.)
 * - 昨日食べた肉かなにかがよくなかったみたい。
 *   (Apparently the meat he ate yesterday was not good or something.)
 *
 * Key discriminators:
 * - Follows nouns (NOUN, PROPN, PRON)
 * - か is a particle (ADP/SCONJ/PART) meaning "or"
 * - 何か/なにか is a pronoun meaning "something"
 * - The entire phrase (か何か/かなにか) expresses uncertainty
 *
 * Different from:
 * - かどうか (whether or not) - expresses uncertainty about a state/action
 * - でも (or something) - more vague, implies one of many possibilities
 * - Simple 何か (something) - without the preceding noun + か pattern
 *
 * GiNZA parse structure:
 * - NOUN + か(ADP/SCONJ/PART) + 何か/なにか(PRON)
 * - Various dependency relations (compound, fixed, list, flat)
 * - When written in hiragana, may be tokenized as single unit or multiple tokens
 */
export default linguisticRule('か何か', (r) => {
  r.either(
    // Pattern 1: Noun + か何か/かなにか (single token)
    // GiNZA sometimes tokenizes the entire phrase as one PRON token
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kananika = b1.tok({
        textOneOf: ['か何か', 'かなにか', 'か何（なに）か', 'か何（なん）か'],
      }, 'kananika');

      b1.inOrder(noun, kananika, 1);
      b1.capture(kananika);
    },

    // Pattern 1b: Noun + か何か/かなにか (single token) with any POS
    // Even more permissive for single-token matches
    (b1b) => {
      const noun = b1b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB', 'ADJ'] }, 'noun');
      const kananika = b1b.tok({
        textOneOf: ['か何か', 'かなにか', 'か何（なに）か', 'か何（なん）か'],
      }, 'kananika');

      b1b.inOrder(noun, kananika, 3);
      b1b.capture(kananika);
    },

    // Pattern 2: Noun + か + 何か/なにか with compound dependency
    // Most common pattern where all three form a compound unit
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka = b2.particle('か', 'ka');
      const nanika = b2.tok({ textOneOf: ['何か', 'なにか'] }, 'nanika');

      b2.inOrder(noun, ka, 1);
      b2.inOrder(ka, nanika, 1);
      b2.headChild(noun, ka, 'compound');
      b2.headChild(noun, nanika, 'compound');

      b2.captureSpan('か何か', noun, nanika);
    },

    // Pattern 3: Noun + か + 何か/なにか with fixed dependency
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka = b3.particle('か', 'ka');
      const nanika = b3.tok({ textOneOf: ['何か', 'なにか'] }, 'nanika');

      b3.inOrder(noun, ka, 1);
      b3.inOrder(ka, nanika, 1);
      b3.headChild(noun, ka, 'fixed');
      b3.headChild(noun, nanika, 'fixed');

      b3.captureSpan('か何か', noun, nanika);
    },

    // Pattern 4: Noun + か + 何か/なにか with list dependency
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka = b4.particle('か', 'ka');
      const nanika = b4.tok({ textOneOf: ['何か', 'なにか'] }, 'nanika');

      b4.inOrder(noun, ka, 1);
      b4.inOrder(ka, nanika, 1);
      b4.headChild(noun, ka, 'list');
      b4.headChild(noun, nanika, 'list');

      b4.captureSpan('か何か', noun, nanika);
    },

    // Pattern 5: Noun + か + 何か/なにか with flat dependency
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka = b5.particle('か', 'ka');
      const nanika = b5.tok({ textOneOf: ['何か', 'なにか'] }, 'nanika');

      b5.inOrder(noun, ka, 1);
      b5.inOrder(ka, nanika, 1);
      b5.headChild(noun, ka, 'flat');
      b5.headChild(noun, nanika, 'flat');

      b5.captureSpan('か何か', noun, nanika);
    },

    // Pattern 6: Noun + か + 何か/なにか (catch-all with loose constraints)
    // For unexpected GiNZA parsings - just require order and proximity
    (b6) => {
      const noun = b6.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka = b6.particle('か', 'ka');
      const nanika = b6.tok({ textOneOf: ['何か', 'なにか'] }, 'nanika');

      b6.inOrder(noun, ka, 3);
      b6.inOrder(ka, nanika, 3);

      b6.captureSpan('か何か', noun, nanika);
    }
  );
});
