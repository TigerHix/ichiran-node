import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: とか (to ka) - I heard that... or something
 *
 * A casual hearsay expression that quotes something heard with uncertainty.
 * Similar to "I heard (A) or something" or "They say (A) or something like that."
 *
 * This is the QUOTATIONAL usage (combining quotative particle と + question particle か),
 * NOT the listing usage (examples like "X, Y, etc.").
 *
 * Structure:
 * - Verb (casual form) + とか
 * - I-adjective + とか
 * - Na-adjective + だ + とか
 * - Noun + だ + とか
 *
 * Examples:
 * - 遠藤さんと近藤さんが結婚するとか。
 *   (I heard Endo-san and Kondo-san are getting married or something.)
 * - 納豆は体にいいんだとか。
 *   (I heard Natto is good for your body or something.)
 * - 高橋くんの彼女が美人だとか。
 *   (I heard Takahashi's girlfriend is beautiful or something.)
 * - 先生がテストの問題は難しくないとか言ってたけど本当かな？
 *   (The teacher was saying the test questions weren't hard or something like that.)
 *
 * Key discriminators:
 * - Appears at end of sentence or before final punctuation
 * - Follows casual form of verb/adj/noun+だ
 * - Expresses uncertainty/heard information
 * - NOT followed by another noun (that would be listing usage)
 * - NOT followed by particles を, で, に, は (that indicate listing)
 * - NOT the quotative particle と followed by question marker か separately
 *
 * GiNZA parse structure:
 * - GiNZA splits とか into TWO tokens: と (ADP) + か (PART/ADP)
 * - と: ADP (助詞-格助詞, lemma=と) - quotative particle
 * - か: PART (助詞-副助詞, lemma=か) - question particle
 *
 * Different from:
 * - Listing とか (noun + とか + noun): "りんごとかバナナを買う" (buy things like apples and bananas)
 * - Simple question か: "行くか？" (Will you go?)
 * - Quotative と alone: "行くと言った" (said (they) will go)
 */
export default linguisticRule('とか', (r) => {
  r.either(
    // Pattern 1: Predicate + と (ADP) + か (PART/ADP) + VERB/PUNCT/SYM
    // This matches quotational usage where とか is followed by speech verbs or punctuation
    // NOT listing usage where か is followed by particles like を, で, は
    // The predicate must be a VERB, ADJ, or AUX (not just a bare NOUN)
    // to exclude listing patterns like "AとかBとか"
    (b1) => {
      const predicate = b1.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'predicate');
      const to = b1.tok({ text: 'と' }, 'to');
      const ka = b1.tok({ text: 'か' }, 'ka');
      // Require speech verb or punctuation/symbol immediately after か (for quotational)
      const afterKa = b1.tok({ posOneOf: ['VERB', 'PUNCT', 'SYM', 'AUX'] }, 'afterKa');

      b1.inOrder(predicate, to, 3);
      b1.inOrder(to, ka, 1);
      // Distance 1 ensures か is immediately followed by VERB/PUNCT/SYM/AUX
      // This excludes listing patterns where か is followed by ADP particles (を, で, は)
      b1.inOrder(ka, afterKa, 1);

      b1.captureSpan('とか', to, ka);
    },

    // Pattern 1b: Noun + だ/です + と + か (na-adjective or noun+copula ending)
    (b1b) => {
      const noun = b1b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = b1b.tok({ textOneOf: ['だ', 'です'] }, 'da');
      const to = b1b.tok({ text: 'と' }, 'to');
      const ka = b1b.tok({ text: 'か' }, 'ka');
      // Require speech verb or punctuation/symbol immediately after か
      const afterKa = b1b.tok({ posOneOf: ['VERB', 'PUNCT', 'SYM', 'AUX'] }, 'afterKa');

      b1b.inOrder(noun, da, 2);
      b1b.inOrder(da, to, 1);
      b1b.inOrder(to, ka, 1);
      b1b.inOrder(ka, afterKa, 1);

      b1b.captureSpan('とか', to, ka);
    },

    // Pattern 2: Combined とか token (when GiNZA doesn't split)
    // This pattern requires VERB/ADJ/AUX before とか to avoid listing patterns
    (b2) => {
      const predicate = b2.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'predicate');
      const toka = b2.tok({ text: 'とか' }, 'toka');
      // Require speech verb or punctuation/symbol immediately after とか
      const afterToka = b2.tok({ posOneOf: ['VERB', 'PUNCT', 'SYM', 'AUX'] }, 'afterToka');

      b2.inOrder(predicate, toka, 3);
      b2.inOrder(toka, afterToka, 1);

      b2.captureSpan('とか', toka, toka);
    },

    // Pattern 2b: Noun + だ/です + とか (combined)
    (b2b) => {
      const noun = b2b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = b2b.tok({ textOneOf: ['だ', 'です'] }, 'da');
      const toka = b2b.tok({ text: 'とか' }, 'toka');
      const afterToka = b2b.tok({ posOneOf: ['VERB', 'PUNCT', 'SYM', 'AUX'] }, 'afterToka');

      b2b.inOrder(noun, da, 2);
      b2b.inOrder(da, toka, 1);
      b2b.inOrder(toka, afterToka, 1);

      b2b.captureSpan('とか', toka, toka);
    },

    // Pattern 3: とか followed by aux verbs (て, た, etc.) then speech verb
    // e.g., "と言ってた" where て is AUX
    // Requires VERB/ADJ/AUX before と to avoid listing patterns
    (b3) => {
      const predicate = b3.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'predicate');
      const to = b3.tok({ text: 'と' }, 'to');
      const ka = b3.tok({ text: 'か' }, 'ka');
      // Allow AUX after か, then require VERB
      const aux = b3.tok({ pos: 'AUX' }, 'aux');
      const verb = b3.tok({ pos: 'VERB' }, 'verb');

      b3.inOrder(predicate, to, 3);
      b3.inOrder(to, ka, 1);
      b3.inOrder(ka, aux, 1);
      b3.inOrder(aux, verb, 1);

      b3.captureSpan('とか', to, ka);
    },

    // Pattern 3b: Noun + だ/です + とか + aux + verb
    (b3b) => {
      const noun = b3b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = b3b.tok({ textOneOf: ['だ', 'です'] }, 'da');
      const to = b3b.tok({ text: 'と' }, 'to');
      const ka = b3b.tok({ text: 'か' }, 'ka');
      const aux = b3b.tok({ pos: 'AUX' }, 'aux');
      const verb = b3b.tok({ pos: 'VERB' }, 'verb');

      b3b.inOrder(noun, da, 2);
      b3b.inOrder(da, to, 1);
      b3b.inOrder(to, ka, 1);
      b3b.inOrder(ka, aux, 1);
      b3b.inOrder(aux, verb, 1);

      b3b.captureSpan('とか', to, ka);
    }
  );
});
