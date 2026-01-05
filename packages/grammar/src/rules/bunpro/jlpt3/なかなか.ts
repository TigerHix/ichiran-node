import { bunproLinguisticRule } from '../../../engine/lang.js';

export default bunproLinguisticRule('なかなか', (r) => {
  // なかなか (nakanaka) - "quite/rather/fairly"
  // An adverb expressing that something is "quite" or "considerably" (A),
  // often implying it exceeds expectations.
  //
  // Patterns:
  // 1. なかなか + Adjective: なかなか美味しい, なかなか大変, なかなか可愛い
  // 2. なかなか + の + Noun: なかなかの物, 中々の出来栄え, なかなかの美人
  //
  // Note: This is the affirmative usage meaning "quite".
  // The negative pattern "なかなか～ない" (not easily/hardly) is a separate grammar point.
  //
  // Also matches verb phrases like なかなか頼りになる, なかなか素直でいい
  // but excludes negative patterns なかなか～ない (tested via negatives).
  //
  // GiNZA parsing notes:
  // - なかなか is typically ADV (adverb) with dep=advmod when modifying predicates
  // - When followed by の, it can be:
  //   * ADV with dep=advmod (なかなかの物だ)
  //   * ADJ with dep=nmod (中々の出来栄え - kanji form parsed as adjective)
  // - lemma can be なかなか or 中々 depending on surface form
  // - It modifies various parts of speech: ADJ, VERB, NOUN (via の)

  r.either(
    // Pattern 1: なかなか + Predicate (adjective, verb, or noun)
    // なかなか美味しい, なかなか大変, なかなか可愛い
    // なかなか頼りになる, なかなか素直でいい (verb phrases)
    // 中々かっこいい
    // Here なかなか has dep=advmod pointing to what it modifies
    (b) => {
      const nakanaka = b.tok({
        pos: 'ADV',
        dep: 'advmod',
        lemmaOneOf: ['なかなか', '中々'],
        textOneOf: ['なかなか', '中々'],
      }, 'nakanaka');
      // Match whatever it modifies (adjacent or within 3 tokens)
      // This captures ADJ, VERB, NOUN, or AUX that なかなか modifies
      const modified = b.tok({
        posOneOf: ['ADJ', 'VERB', 'NOUN', 'AUX'],
      }, 'modified');
      b.inOrder(nakanaka, modified, 3);
      b.captureSpan('なかなか', nakanaka, modified);
    },

    // Pattern 2: なかなか + の + Noun (noun phrase with adjectival use)
    // なかなかの物, 中々の出来栄え, なかなかの美人, なかなかの事
    // Here なかなか can be ADV (with dep=advmod) or ADJ (with dep=nmod)
    (b) => {
      const nakanaka = b.tok({
        posOneOf: ['ADV', 'ADJ'],
        lemmaOneOf: ['なかなか', '中々'],
        textOneOf: ['なかなか', '中々'],
      }, 'nakanaka');
      const no = b.particle('の', 'no');
      const noun = b.tok({
        posOneOf: ['NOUN', 'PROPN'],
      }, 'noun');
      b.inOrder(nakanaka, no, 1);
      b.inOrder(no, noun, 1);
      b.captureSpan('なかなか', nakanaka, noun);
    }
  );
});
