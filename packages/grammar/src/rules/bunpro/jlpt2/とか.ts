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
 * - NOT the quotative particle と followed by question marker か separately
 *
 * GiNZA parse structure:
 * - とか often parsed as PART or ADP
 * - May have dep=mark (sentence-final particle)
 * - Sometimes combined with preceding token (e.g., "るとか" as one token)
 *
 * Different from:
 * - Listing とか (noun + とか + noun): "りんごとかバナナを買う" (buy things like apples and bananas)
 * - Simple question か: "行くか？" (Will you go?)
 * - Quotative と alone: "行くと言った" (said (they) will go)
 */
export default linguisticRule('とか', (r) => {
  r.either(
    // Pattern 1: Match standalone とか particle
    (b1) => {
      const predicate = b1.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON', 'AUX'] }, 'predicate');
      const toka = b1.tok({ text: 'とか' }, 'toka');

      b1.inOrder(predicate, toka, 30);
      b1.captureSpan('とか', predicate, toka);
    },

    // Pattern 2: Match とか combined with る (verb ending)
    // e.g., "する" + "とか" = "するとか" or "るとか"
    (b2) => {
      const verb = b2.verb({}, 'verb');
      const ru = b2.tok({ textOneOf: ['る', 'るとか', 'るとか。', 'るとか…'] }, 'ru');

      b2.inOrder(verb, ru, 3);
      b2.captureSpan('とか', verb, ru);
    },

    // Pattern 3: Match だ + とか combined
    // e.g., "だとか", "だとか。"
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON', 'ADJ'] }, 'noun');
      const dator = b3.tok({ textOneOf: ['だとか', 'だとか。', 'だとか…'] }, 'dator');

      b3.inOrder(noun, dator, 5);
      b3.captureSpan('とか', noun, dator);
    },

    // Pattern 4: Match ん + だ + とか combined
    // e.g., "んだとか", "んだとか。"
    (b4) => {
      const predicate = b4.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON'] }, 'predicate');
      const nda = b4.aux({ textOneOf: ['ん', 'の'] }, 'n');
      const ndator = b4.tok({ textOneOf: ['だとか', 'だとか。', 'だとか…'] }, 'ndator');

      b4.inOrder(predicate, nda, 3);
      b4.inOrder(nda, ndator, 3);
      b4.captureSpan('とか', predicate, ndator);
    },

    // Pattern 5: Match token that contains とか followed by punctuation/ellipsis
    // e.g., "うるとか。", "しうるとか…"
    (b5) => {
      const predicate = b5.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON', 'AUX'] }, 'predicate');
      const ending = b5.tok({ textOneOf: ['るとか。', 'るとか…', 'だとか。', 'だとか…', 'んだとか。', 'んだとか…', 'うるとか。', 'うるとか…'] }, 'ending');

      b5.inOrder(predicate, ending, 10);
      b5.captureSpan('とか', predicate, ending);
    }
  );
});
