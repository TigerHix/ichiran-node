import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: か何か (ka nanika) - "or something, or something like that"
 *
 * A casual phrase which expresses whether it is (A), or whatever it is,
 * the speaker is not sure. Used to indicate uncertainty or vagueness.
 *
 * Structure: Noun + か何か (hiragana: かなにか)
 *
 * Examples:
 * - お茶かなにかありませんか？
 *   (Do you have tea or something?)
 * - ビールかなにかがほしいな。
 *   (I'd like a beer or something.)
 * - 学校かなにかに通ってるみたいだけど。
 *   (He seems to go to school or something.)
 * - 交通渋滞かなにかのような気がする。
 *   (I have a feeling it's traffic or something.)
 *
 * Key discriminators:
 * - Follows a noun (NOUN, PROPN, PRON)
 * - First か is a particle (PART/ADP)
 * - 何 is the indefinite pronoun "what/something" (NOUN/PRON)
 * - Second か is a particle (PART/ADP)
 * - The entire phrase means "noun + or something"
 *
 * GiNZA parse structure:
 * - Noun + か(PART) + 何(NOUN/PRON) + か(PART)
 * - Can parse as compound, fixed, or loose dependencies
 *
 * Pronunciation variants:
 * - か何か (ka nanika) - standard form
 * - かなにか (ka nanika) - hiragana form (common)
 * - かなんか (ka nanka) - casual/colloquial form
 *
 * Different from:
 * - かどうか (ka dou ka) - "whether or not" (uncertainty marker)
 * - かも (ka mo) - "might be" (possibility)
 * - かというと (ka to iu to) - "as for" (topic marker)
 * - でも (demo) - "or something" (vaguer, less specific)
 */
export default linguisticRule('か何か', (r) => {
  r.either(
    // Pattern 1: Noun + か(PART) + 何(NOUN) + か(PART) - standard parsing
    // Most common pattern with part of speech tags
    (b1) => {
      const noun = b1.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka1 = b1.particle('か', 'ka1', { pos: 'PART' });
      const nani = b1.tok({ textOneOf: ['何', 'なに'], posOneOf: ['NOUN', 'PRON'] }, 'nani');
      const ka2 = b1.particle('か', 'ka2', { pos: 'PART' });

      b1.inOrder(noun, ka1, 1);
      b1.inOrder(ka1, nani, 1);
      b1.inOrder(nani, ka2, 1);

      b1.captureSpan('か何か', noun, ka2);
    },

    // Pattern 2: Noun + か(ADP) + 何(NOUN) + か(ADP)
    // Alternative POS tagging for particles as ADP
    (b2) => {
      const noun = b2.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka1 = b2.particle('か', 'ka1', { pos: 'ADP' });
      const nani = b2.tok({ textOneOf: ['何', 'なに'], posOneOf: ['NOUN', 'PRON'] }, 'nani');
      const ka2 = b2.particle('か', 'ka2', { pos: 'ADP' });

      b2.inOrder(noun, ka1, 1);
      b2.inOrder(ka1, nani, 1);
      b2.inOrder(nani, ka2, 1);

      b2.captureSpan('か何か', noun, ka2);
    },

    // Pattern 3: Noun + か + 何 + か with compound dependencies
    // GiNZA sometimes parses this as a compound structure
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka1 = b3.particle('か', 'ka1');
      const nani = b3.tok({ textOneOf: ['何', 'なに'], posOneOf: ['NOUN', 'PRON'] }, 'nani');
      const ka2 = b3.particle('か', 'ka2');

      b3.inOrder(noun, ka1, 1);
      b3.inOrder(ka1, nani, 1);
      b3.inOrder(nani, ka2, 1);
      b3.headChild(noun, ka1, 'compound');
      b3.headChild(ka1, nani, 'compound');
      b3.headChild(nani, ka2, 'compound');

      b3.captureSpan('か何か', noun, ka2);
    },

    // Pattern 4: Noun + か + 何 + か with fixed dependencies
    // Fixed expression pattern
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka1 = b4.particle('か', 'ka1');
      const nani = b4.tok({ textOneOf: ['何', 'なに'], posOneOf: ['NOUN', 'PRON'] }, 'nani');
      const ka2 = b4.particle('か', 'ka2');

      b4.inOrder(noun, ka1, 1);
      b4.inOrder(ka1, nani, 1);
      b4.inOrder(nani, ka2, 1);
      b4.headChild(noun, ka1, 'fixed');
      b4.headChild(ka1, nani, 'fixed');
      b4.headChild(nani, ka2, 'fixed');

      b4.captureSpan('か何か', noun, ka2);
    },

    // Pattern 5: Noun + か + 何 + か (catch-all with loose constraints)
    // For unexpected GiNZA parsings
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka1 = b5.particle('か', 'ka1');
      const nani = b5.tok({ textOneOf: ['何', 'なに'], posOneOf: ['NOUN', 'PRON'] }, 'nani');
      const ka2 = b5.particle('か', 'ka2');

      b5.inOrder(noun, ka1, 1);
      b5.inOrder(ka1, nani, 1);
      b5.inOrder(nani, ka2, 1);

      b5.captureSpan('か何か', noun, ka2);
    },

    // Pattern 6: Noun + compound "かなにか" (single token)
    // GiNZA sometimes parses the entire "かなにか" as a single ADV/NOUN token
    (b6) => {
      const noun = b6.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const nanika = b6.tok({ textOneOf: ['かなにか', 'かなんか'], posOneOf: ['ADV', 'NOUN', 'PRON'] }, 'nanika');

      b6.inOrder(noun, nanika, 1);

      b6.captureSpan('か何か', noun, nanika);
    },

    // Pattern 7: Noun + compound "何か" (single token after noun + ka)
    // Noun + か + compound何か
    (b7) => {
      const noun = b7.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka1 = b7.particle('か', 'ka1');
      const nanika = b7.tok({ textOneOf: ['何か', 'なにか'], posOneOf: ['NOUN', 'PRON'] }, 'nanika');

      b7.inOrder(noun, ka1, 1);
      b7.inOrder(ka1, nanika, 1);

      b7.captureSpan('か何か', noun, nanika);
    }
  );
});
