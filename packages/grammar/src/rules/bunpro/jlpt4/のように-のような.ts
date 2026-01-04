import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT4: のように・のような (like/similar to)
 *
 * Noun + のように/のような = "like X" / "similar to X"
 *
 * - ように modifies verbs, adjectives, or acts as adverb
 * - ような modifies nouns (adnominal form)
 *
 * Key patterns:
 * 1. Noun + の + よう + に (modifies verb/adj)
 * 2. Noun + の + よう + な (modifies noun)
 * 3. Demonstrative (この/その) + よう + に (no particle)
 * 4. Demonstrative (この/その) + よう + な (no particle)
 *
 * Examples:
 * - 妹が孫のように甘える (My sister clings like a grandchild)
 * - スープのようなカレー (Curry like soup)
 * - この先生は鬼のように怖い (This teacher is scary like a demon)
 * - お箸はこのように使います (Use chopsticks like this)
 * - このような洋服 (Clothes like this)
 *
 * GiNZA parse structure:
 * - "のように" typically parses as: の(ADP) + よう(NOUN/AUX) + に(ADP)
 * - "のような" typically parses as: の(ADP) + よう(NOUN/AUX) + な(AUX)
 * - Demonstratives: この/その(DET) + よう(NOUN) + に/な
 */
export default linguisticRule('のように-のような', (r) => {
  r.either(
    // Pattern 1: Noun + の + よう + に (modifies verb/adj)
    (b) => {
      // Allow NOUN or PRON (for pronouns like 君)
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON'],
      }, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      const you = b.tok({
        lemma: 'よう',
      }, 'you');
      b.inOrder(no, you, 1);

      const ni = b.tok({
        text: 'に',
      }, 'ni');
      b.inOrder(you, ni, 1);

      b.captureSpan('のように', noun, ni);
    },

    // Pattern 2: Noun + の + よう + な (modifies noun)
    (b) => {
      // Allow NOUN or PRON (for pronouns like 君)
      const noun = b.tok({
        posOneOf: ['NOUN', 'PRON'],
      }, 'noun');
      const no = b.particle('の', 'no');
      b.inOrder(noun, no, 1);

      const you = b.tok({
        lemma: 'よう',
      }, 'you');
      b.inOrder(no, you, 1);

      const na = b.tok({
        text: 'な',
      }, 'na');
      b.inOrder(you, na, 1);

      b.captureSpan('のような', noun, na);
    },

    // Pattern 3: Demonstrative (この/その) + よう + に (no particle)
    (b) => {
      const demo = b.tok({
        pos: 'DET',
      }, 'demo');

      const you = b.tok({
        lemma: 'よう',
      }, 'you');
      b.inOrder(demo, you, 1);

      const ni = b.tok({
        text: 'に',
      }, 'ni');
      b.inOrder(you, ni, 1);

      b.captureSpan('のように', demo, ni);
    },

    // Pattern 4: Demonstrative (この/その) + よう + な (no particle)
    (b) => {
      const demo = b.tok({
        pos: 'DET',
      }, 'demo');

      const you = b.tok({
        lemma: 'よう',
      }, 'you');
      b.inOrder(demo, you, 1);

      const na = b.tok({
        text: 'な',
      }, 'na');
      b.inOrder(you, na, 1);

      b.captureSpan('のような', demo, na);
    }
  );
});
