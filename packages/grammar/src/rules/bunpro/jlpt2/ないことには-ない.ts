import { linguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: ないことには-ない (nai koto niwa nai) - cannot... unless...
 *
 * A grammar pattern meaning "cannot do Y unless X" or "without X, cannot Y".
 * This expresses that (B) cannot happen or exist unless (A) happens first.
 *
 * Structure:
 * - Verb-negative + こと + には + verb-negative
 * - I-adjective-negative + こと + には + verb-negative
 * - Na-adjective/Noun + でない + こと + には + verb-negative
 *
 * The pattern consists of two parts connected by ことには:
 * 1. The condition (negative form)
 * 2. The consequence (also negative)
 *
 * Examples:
 * - ヘルメットを被らないことには、この工事現場には入れない。
 *   (Unless you wear a helmet, you cannot enter this construction site.)
 * - ジェットコースターは、スリルがないことには楽しくない。
 *   (Roller coasters are no fun unless they are thrilling.)
 * - 何事も一生懸命でないことには、何も上手くなれない。
 *   (You won't get good at anything unless you try your best.)
 * - やってみないことには、出来るかどうか分からない。
 *   (You won't know if you can do it unless you try.)
 *
 * Key discriminators:
 * - First part ends in negative form (ない, 〜ない, 〜でない)
 * - Followed by ことには (koto niwa - nominalizer + topic particle)
 * - Second clause ends in negative verb (ない, ません, etc.)
 * - こと is a noun (NOUN) nominalizing the preceding clause
 * - に is the case particle (ADP)
 * - は is the topic particle (ADP/PART)
 *
 * Different from:
 * - ことには without negative (different meaning)
 * - Simple こと + に (not a grammar pattern)
 * - ないで (without doing - different)
 * - ないまま (in the state of not being - different)
 * - ないかぎり (unless - similar but different structure)
 *
 * GiNZA parse structure:
 * - First negative may be VERB+AUX, ADJ+AUX, or NOUN+copula
 * - こと is NOUN
 * - に is ADP (case marker)
 * - は is ADP or PART (topic marker)
 * - Final negative is verb with auxiliary ない/ません
 */
export default linguisticRule('ないことには-ない', (r) => {
  r.either(
    // Pattern 1: Flexible predicate + ことには + verb-negative (NO WA)
    // Handles sentences without は particle after ことには
    (b1) => {
      const predicate1 = b1.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'predicate1');
      const koto = b1.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b1.tok({ text: 'に' }, 'ni');
      const masen = b1.aux({ lemma: 'ます' }, 'masen');  // Match polite negative ending

      b1.inOrder(predicate1, koto, 10);
      b1.inOrder(koto, ni, 3);
      b1.inOrder(ni, masen, 30);

      b1.captureSpan('ないことには-ない', predicate1, masen);
    },

    // Pattern 1b: Flexible predicate + ことには + verb-negative (WITH WA)
    // Handles most cases with various tokenizations
    (b1b) => {
      const predicate1 = b1b.tok({ posOneOf: ['VERB', 'ADJ', 'AUX'] }, 'predicate1');
      const koto = b1b.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b1b.tok({ text: 'に' }, 'ni');
      const wa = b1b.tok({ textOneOf: ['は', 'ワ'] }, 'wa');
      const predicate2 = b1b.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'predicate2');
      const nai2 = b1b.tok({ textOneOf: ['ない', 'ぬ', 'ません', 'ませんでした'] }, 'nai2');

      b1b.inOrder(predicate1, koto, 10);
      b1b.inOrder(koto, ni, 3);
      b1b.inOrder(ni, wa, 10);
      b1b.inOrder(wa, predicate2, 20);
      b1b.inOrder(predicate2, nai2, 10);

      b1b.captureSpan('ないことには-ない', predicate1, nai2);
    },

    // Pattern 2: VERB + AUX (lemma) split + ことには + verb-negative (NO WA)
    (b2) => {
      const verb1 = b2.verb({}, 'verb1');
      const nai1 = b2.aux({ lemmaOneOf: ['ない', 'ぬ'] }, 'nai1');
      const koto = b2.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b2.tok({ text: 'に' }, 'ni');
      const nai2 = b2.tok({ textOneOf: ['ない', 'ぬ', 'ません'] }, 'nai2');

      b2.inOrder(verb1, nai1, 3);
      b2.inOrder(nai1, koto, 5);
      b2.inOrder(koto, ni, 3);
      b2.inOrder(ni, nai2, 30);  // Skip wa, skip predicate2, go directly to nai2

      b2.captureSpan('ないことには-ない', verb1, nai2);
    },

    // Pattern 2b: VERB + AUX (lemma) split + ことには + verb-negative (WITH WA)
    // Handles cases where verb and auxiliary are split
    (b2b) => {
      const verb1 = b2b.verb({}, 'verb1');
      const nai1 = b2b.aux({ lemmaOneOf: ['ない', 'ぬ'] }, 'nai1');
      const koto = b2b.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b2b.tok({ text: 'に' }, 'ni');
      const wa = b2b.tok({ textOneOf: ['は', 'ワ'] }, 'wa');
      const predicate2 = b2b.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'predicate2');
      const nai2 = b2b.tok({ textOneOf: ['ない', 'ぬ', 'ません'] }, 'nai2');

      b2b.inOrder(verb1, nai1, 3);
      b2b.inOrder(nai1, koto, 5);
      b2b.inOrder(koto, ni, 3);
      b2b.inOrder(ni, wa, 10);
      b2b.inOrder(wa, predicate2, 20);
      b2b.inOrder(predicate2, nai2, 10);

      b2b.captureSpan('ないことには-ない', verb1, nai2);
    },

    // Pattern 3: VERB + AUX (text) split + ことには + verb-negative (NO WA)
    (b3) => {
      const verb1 = b3.verb({}, 'verb1');
      const nai1 = b3.aux({ text: 'ない' }, 'nai1');
      const koto = b3.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b3.tok({ text: 'に' }, 'ni');
      const nai2 = b3.tok({ textOneOf: ['ない', 'ぬ', 'ません'] }, 'nai2');

      b3.inOrder(verb1, nai1, 3);
      b3.inOrder(nai1, koto, 5);
      b3.inOrder(koto, ni, 3);
      b3.inOrder(ni, nai2, 30);  // Skip wa, skip predicate2, go directly to nai2

      b3.captureSpan('ないことには-ない', verb1, nai2);
    },

    // Pattern 3b: VERB + AUX (text) split + ことには + verb-negative (WITH WA)
    // Handles cases where auxiliary is identified by text not lemma
    (b3b) => {
      const verb1 = b3b.verb({}, 'verb1');
      const nai1 = b3b.aux({ text: 'ない' }, 'nai1');
      const koto = b3b.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b3b.tok({ text: 'に' }, 'ni');
      const wa = b3b.tok({ textOneOf: ['は', 'ワ'] }, 'wa');
      const predicate2 = b3b.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'predicate2');
      const nai2 = b3b.tok({ textOneOf: ['ない', 'ぬ', 'ません'] }, 'nai2');

      b3b.inOrder(verb1, nai1, 3);
      b3b.inOrder(nai1, koto, 5);
      b3b.inOrder(koto, ni, 3);
      b3b.inOrder(ni, wa, 10);
      b3b.inOrder(wa, predicate2, 20);
      b3b.inOrder(predicate2, nai2, 10);

      b3b.captureSpan('ないことには-ない', verb1, nai2);
    },

    // Pattern 4: Noun + でない + ことには + verb-negative
    // e.g., 一生懸命でないことには, スリルがないことには (when parsed as noun)
    (b4) => {
      const noun = b4.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const denai = b4.tok({ text: 'でない' }, 'denai');
      const koto = b4.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b4.tok({ text: 'に' }, 'ni');
      const wa = b4.tok({ textOneOf: ['は', 'ワ'] }, 'wa');
      const predicate2 = b4.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'predicate2');
      const nai2 = b4.tok({ textOneOf: ['ない', 'ぬ', 'ません'] }, 'nai2');

      b4.inOrder(noun, denai, 3);
      b4.inOrder(denai, koto, 5);
      b4.inOrder(koto, ni, 3);
      b4.inOrder(ni, wa, 10);
      b4.inOrder(wa, predicate2, 20);
      b4.inOrder(predicate2, nai2, 10);

      b4.captureSpan('ないことには-ない', noun, nai2);
    },

    // Pattern 5: I-adj + ない + ことには + verb-negative
    // Handles i-adjective negative forms
    (b5) => {
      const adj = b5.adj({}, 'adj');
      const nai1 = b5.aux({ text: 'ない' }, 'nai1');
      const koto = b5.noun({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b5.tok({ text: 'に' }, 'ni');
      const wa = b5.tok({ textOneOf: ['は', 'ワ'] }, 'wa');
      const predicate2 = b5.tok({ posOneOf: ['VERB', 'AUX', 'ADJ'] }, 'predicate2');
      const nai2 = b5.tok({ textOneOf: ['ない', 'ぬ', 'ません'] }, 'nai2');

      b5.inOrder(adj, nai1, 3);
      b5.inOrder(nai1, koto, 5);
      b5.inOrder(koto, ni, 3);
      b5.inOrder(ni, wa, 10);
      b5.inOrder(wa, predicate2, 20);
      b5.inOrder(predicate2, nai2, 10);

      b5.captureSpan('ないことには-ない', adj, nai2);
    },

    // Pattern 6: Ultra-loose - any token before ことには + any negative
    // Catches remaining edge cases with very flexible matching
    (b6) => {
      const token1 = b6.tok({ posOneOf: ['VERB', 'ADJ', 'AUX', 'NOUN', 'PROPN', 'PRON', 'PART', 'SCONJ', 'ADP'] }, 'token1');
      const koto = b6.tok({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b6.tok({ text: 'に' }, 'ni');
      const wa = b6.tok({ textOneOf: ['は', 'ワ'] }, 'wa');
      const token2 = b6.tok({ textOneOf: ['ない', 'ぬ', 'ません', 'ませんでした', 'ないだろう', 'ないでしょう'] }, 'token2');

      b6.inOrder(token1, koto, 20);
      b6.inOrder(koto, ni, 5);
      b6.inOrder(ni, wa, 10);  // Increased distance to allow for comma
      b6.inOrder(wa, token2, 40);

      b6.captureSpan('ないことには-ない', token1, token2);
    },

    // Pattern 7: Same but without requiring wa (は) - for sentences like "しないことには先方に"
    (b7) => {
      const token1 = b7.tok({ posOneOf: ['VERB', 'ADJ', 'AUX', 'NOUN', 'PROPN', 'PRON'] }, 'token1');
      const koto = b7.tok({ textOneOf: ['こと', 'コト'] }, 'koto');
      const ni = b7.tok({ text: 'に' }, 'ni');
      const token2 = b7.tok({ textOneOf: ['ない', 'ぬ', 'ません', 'ませんでした', 'ないだろう', 'ないでしょう'] }, 'token2');

      b7.inOrder(token1, koto, 20);
      b7.inOrder(koto, ni, 5);
      b7.inOrder(ni, token2, 35);  // Skip wa, go directly to negative

      b7.captureSpan('ないことには-ない', token1, token2);
    }
  );
});
