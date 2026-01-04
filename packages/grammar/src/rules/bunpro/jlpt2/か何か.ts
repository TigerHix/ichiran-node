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
 * GiNZA parse structure variations:
 * - Noun + か(PART) + 何(NOUN) + か(PART) - standard 4-token parse
 * - Noun + かなにか (single token) - compound form
 * - Noun + か + 何か (3 tokens) - 何か is single token
 *
 * Pronunciation variants:
 * - か何か (ka nanika) - standard form with kanji
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
    // Pattern 1: Noun + か(PART) + 何(NOUN/PRON) + か(PART) - standard 4-token parsing
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

    // Pattern 2: Noun + か(ADP) + 何(NOUN/PRON) + か(ADP)
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

    // Pattern 3: Noun + compound "かなにか" (single token)
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const nanika = b3.tok({
        textOneOf: ['かなにか', 'かなんか', 'か何か'],
        posOneOf: ['ADV', 'NOUN', 'PRON', 'PART', 'ADP']
      }, 'nanika');

      b3.inOrder(noun, nanika, 1);
      b3.captureSpan('か何か', noun, nanika);
    },

    // Pattern 4: Noun + か + 何か (3-token form where 何か is single token)
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const ka1 = b4.tok({ text: 'か', posOneOf: ['PART', 'ADP'] }, 'ka1');
      const nanika = b4.tok({
        textOneOf: ['何か', 'なにか'],
        posOneOf: ['NOUN', 'PRON', 'PART', 'ADP', 'ADV']
      }, 'nanika');

      b4.inOrder(noun, ka1, 1);
      b4.inOrder(ka1, nanika, 1);
      b4.captureSpan('か何か', noun, nanika);
    },

    // Pattern 5: Noun + 何か (direct form, implicit first ka)
    (b5) => {
      const noun = b5.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const nanika = b5.tok({
        textOneOf: ['何か', 'なにか'],
        posOneOf: ['NOUN', 'PRON', 'ADV']
      }, 'nanika');

      b5.inOrder(noun, nanika, 1);
      b5.captureSpan('か何か', noun, nanika);
    }
  );
});
