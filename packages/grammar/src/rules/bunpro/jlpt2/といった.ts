import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: といった (to itta) - "such as, like"
 *
 * A grammar pattern meaning "such as" or "like" when giving examples.
 * It emphasizes things that belong to a category and then explains them.
 *
 * Structure:
 * - Noun + といった + Noun
 * - Examples: A + といった + B (B such as A)
 *
 * Examples:
 * - 素質といったものは、遺伝的な要素が大きいのかもしれない。
 *   (Things such as one's character are maybe largely due to genetics.)
 * - 炊事洗濯といった家事もりっぱな仕事だと思います。
 *   (I think doing housework, such as cooking and washing, is a splendid job.)
 * - 蕎麦といった麺類があまり好きではありません。
 *   (I don't really like noodles such as soba.)
 * - 相撲といった日本の文化に興味がある。
 *   (I have interest in Japanese culture such as sumo.)
 *
 * Key discriminators:
 * - Follows a noun (the example being given)
 * - Followed by a noun (the category)
 * - とした is composed of と (quotative particle) + いう (VERB) + た (AUX - past tense)
 * - The pattern groups examples into a category
 * - CRITICAL: Must have た auxiliary to distinguish from という (called/known as)
 *
 * GiNZA parse structure:
 * - First noun: NOUN/PROPN/PRON (may be complex phrase/quote)
 * - と: ADP/PART (quotative particle, dep=case)
 * - いう/いっ: VERB (lemma=いう, dep=fixed)
 * - た: AUX (lemma=た, dep=fixed) - THIS IS THE KEY DISCRIMINATOR
 * - Second noun: NOUN/PROPN/PRON (the category)
 *
 * Different from:
 * - という (toiū) - "called" or "known as" (NO た auxiliary after いう)
 * - など (nado) - "etc." (can stand alone, とした always between nouns)
 * - とか (toka) - "things like" (lists examples separately)
 * - なんて (nante) - more dismissive/emotional, seldom followed by noun
 */
export default linguisticRule('といった', (r) => {
  r.either(
    // Pattern 1: Noun + といった + Noun (split tokens, most precise)
    // e.g., 素質といったもの, 炊事洗濯といった家事, 蕎麦といった麺類
    // CRITICAL: Requires た auxiliary to distinguish from という
    (b1) => {
      const noun1 = b1.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun1');
      const to = b1.particle('と', 'to');
      const iu = b1.tok({
        lemma: 'いう',
        pos: 'VERB',
      }, 'iu');
      const ta = b1.aux({
        lemma: 'た',
      }, 'ta');
      const noun2 = b1.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun2');

      b1.inOrder(noun1, to, 5);
      b1.inOrder(to, iu, 1);
      b1.inOrder(iu, ta, 1);
      b1.inOrder(ta, noun2, 2);

      b1.captureSpan('といった', noun1, noun2);
    },

    // Pattern 2: Noun + といった + Noun (iu verb may have text=いっ or いう)
    // More flexible verb matching
    (b2) => {
      const noun1 = b2.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'],
      }, 'noun1');
      const to = b2.particle('と', 'to');
      const iu = b2.tok({
        pos: 'VERB',
      }, 'iu');
      const ta = b2.aux({
        lemma: 'た',
      }, 'ta');
      const noun2 = b2.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun2');

      b2.inOrder(noun1, to, 10);
      b2.inOrder(to, iu, 1);
      b2.inOrder(iu, ta, 1);
      b2.inOrder(ta, noun2, 3);

      b2.captureSpan('といった', noun1, noun2);
    },

    // Pattern 3: Noun + といった + Noun (single token for といった)
    // Sometimes とした is parsed as one token
    (b3) => {
      const noun1 = b3.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun1');
      const toitta = b3.tok({
        text: 'といった',
      }, 'toitta');
      const noun2 = b3.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun2');

      b3.inOrder(noun1, toitta, 5);
      b3.inOrder(toitta, noun2, 2);

      b3.captureSpan('といった', noun1, noun2);
    },

    // Pattern 4: Loose pattern - first element can be any type (quotes, phrases)
    // with と + verb + た + noun structure
    (b4) => {
      const noun1 = b4.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON', 'VERB'],
      }, 'noun1');
      const to = b4.tok({
        text: 'と',
        pos: 'ADP',
      }, 'to');
      const iu = b4.tok({
        posOneOf: ['VERB', 'AUX'],
      }, 'iu');
      const ta = b4.tok({
        posOneOf: ['AUX', 'VERB'],
        textOneOf: ['た', 'った'],
      }, 'ta');
      const noun2 = b4.tok({
        posOneOf: ['NOUN', 'PROPN', 'PRON'],
      }, 'noun2');

      b4.inOrder(noun1, to, 15);
      b4.inOrder(to, iu, 2);
      b4.inOrder(iu, ta, 2);
      b4.inOrder(ta, noun2, 5);

      b4.captureSpan('といった', noun1, noun2);
    }
  );
});
