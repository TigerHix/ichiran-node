import { bunproLinguisticRule } from '../../../engine/lang.js';

/**
 * JLPT2: からといって (kara to itte) - Just because... doesn't mean
 *
 * A grammar pattern meaning "just because (A) doesn't mean (B)" or "not necessarily
 * because (A)". It indicates that while (A) may be true, it doesn't necessarily
 * lead to (B). Often used for criticism or expressing strong opinions.
 *
 * Structure:
 * - Verb (any form) + からといって
 * - I-adjective + からといって
 * - Na-adjective/Noun + だ + からといって
 *
 * The shortened forms からって (colloquial) and からとて (literary) are also used.
 *
 * Examples:
 * - 安いからといって買いすぎてしまった。
 *   (Just because it's cheap, I bought too much.)
 * - 日本人だからといって、漢字を書けるとは限らない。
 *   (Just because someone is Japanese doesn't mean they can write kanji.)
 * - 暑いからって、そんなに休憩ばかりしていたら仕事が進まないだろ。
 *   (If you keep taking breaks just because it's hot, you won't get work done.)
 *
 * Key discriminators:
 * - Follows verbs, adjectives, or noun+だ
 * - から is the causal conjunction particle (ADP/SCONJ)
 * - と is the quotative particle
 * - いって is the te-form of 言う (to say)
 * - Different from simple から (because) + と言う (to say)
 * - The pattern forms a fixed expression
 *
 * GiNZA parse structure:
 * - Various POS tags for から (ADP, SCONJ, PART)
 * - と as ADP or PART
 * - いって as VERB or AUX (or sometimes without specific inflection form)
 * - Often has compound, fixed, or mark dependencies
 *
 * Different from:
 * - から alone (because/from)
 * - と言って (called/say - quotative)
 * - からして (judging from)
 * - からすると (more objective judgment)
 */
export default bunproLinguisticRule('からといって', (r) => {
  r.either(
    // Pattern 1: Verb + からといって (full form, split tokens)
    // e.g., 慰められたからといって、したからといって、行ったからといって
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const kara = b1.particle('から', 'kara');
      const to = b1.particle('と', 'to');
      const itte = b1.tok({ text: 'いって' }, 'itte');

      b1.inOrder(verb, kara, 5);
      b1.inOrder(kara, to, 1);
      b1.inOrder(to, itte, 1);

      b1.captureSpan('からといって', verb, itte);
    },

    // Pattern 1b: Verb + からといって (with lemma constraint)
    (b1b) => {
      const verb = b1b.verb({}, 'verb');
      const kara = b1b.particle('から', 'kara');
      const to = b1b.particle('と', 'to');
      const itte = b1b.tok({ lemma: '言う' }, 'itte');

      b1b.inOrder(verb, kara, 5);
      b1b.inOrder(kara, to, 1);
      b1b.inOrder(to, itte, 1);

      b1b.captureSpan('からといって', verb, itte);
    },

    // Pattern 1c: Verb + から + といって (single token for といって)
    // Sometimes といって is parsed as a single token
    (b1c) => {
      const verb = b1c.verb({}, 'verb');
      const kara = b1c.particle('から', 'kara');
      const toitte = b1c.tok({ text: 'といって' }, 'toitte');

      b1c.inOrder(verb, kara, 5);
      b1c.inOrder(kara, toitte, 1);

      b1c.captureSpan('からといって', verb, toitte);
    },

    // Pattern 1d: Verb + からといって (single combined token)
    // Sometimes the entire pattern is one token
    (b1d) => {
      const verb = b1d.verb({}, 'verb');
      const karatoitte = b1d.tok({ text: 'からといって' }, 'karatoitte');

      b1d.inOrder(verb, karatoitte, 5);
      b1d.captureSpan('からといって', verb, karatoitte);
    },

    // Pattern 2: I-adjective + からといって (full form, split tokens)
    // e.g., 貧しいからといって、安いからといって、懐かしいからといって
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const kara = b2.particle('から', 'kara');
      const to = b2.particle('と', 'to');
      const itte = b2.tok({ text: 'いって' }, 'itte');

      b2.inOrder(adj, kara, 5);
      b2.inOrder(kara, to, 1);
      b2.inOrder(to, itte, 1);

      b2.captureSpan('からといって', adj, itte);
    },

    // Pattern 2b: I-adjective + からといって (with lemma constraint)
    (b2b) => {
      const adj = b2b.adj({}, 'adj');
      const kara = b2b.particle('から', 'kara');
      const to = b2b.particle('と', 'to');
      const itte = b2b.tok({ lemma: '言う' }, 'itte');

      b2b.inOrder(adj, kara, 5);
      b2b.inOrder(kara, to, 1);
      b2b.inOrder(to, itte, 1);

      b2b.captureSpan('からといって', adj, itte);
    },

    // Pattern 2c: I-adjective + からといって (single token)
    (b2c) => {
      const adj = b2c.adj({}, 'adj');
      const karatoitte = b2c.tok({ text: 'からといって' }, 'karatoitte');

      b2c.inOrder(adj, karatoitte, 5);
      b2c.captureSpan('からといって', adj, karatoitte);
    },

    // Pattern 2d: I-adjective + から + といって
    (b2d) => {
      const adj = b2d.adj({}, 'adj');
      const kara = b2d.particle('から', 'kara');
      const toitte = b2d.tok({ text: 'といって' }, 'toitte');

      b2d.inOrder(adj, kara, 5);
      b2d.inOrder(kara, toitte, 1);

      b2d.captureSpan('からといって', adj, toitte);
    },

    // Pattern 3: Na-adjective/Noun + だ + からといって (full form, split tokens)
    // e.g., 生意気だからといって、丈夫だからといって、日本人だからといって
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = b3.aux({ lemma: 'だ' }, 'da');
      const kara = b3.particle('から', 'kara');
      const to = b3.particle('と', 'to');
      const itte = b3.tok({ text: 'いって' }, 'itte');

      b3.inOrder(noun, da, 2);
      b3.inOrder(da, kara, 2);
      b3.inOrder(kara, to, 1);
      b3.inOrder(to, itte, 1);

      b3.captureSpan('からといって', noun, itte);
    },

    // Pattern 3b: Na-adjective/Noun + だ + からといって (with lemma constraint)
    (b3b) => {
      const noun = b3b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = b3b.aux({ lemma: 'だ' }, 'da');
      const kara = b3b.particle('から', 'kara');
      const to = b3b.particle('と', 'to');
      const itte = b3b.tok({ lemma: '言う' }, 'itte');

      b3b.inOrder(noun, da, 2);
      b3b.inOrder(da, kara, 2);
      b3b.inOrder(kara, to, 1);
      b3b.inOrder(to, itte, 1);

      b3b.captureSpan('からといって', noun, itte);
    },

    // Pattern 3c: Na-adjective/Noun + だ + からといって (optional da)
    // Sometimes だ is omitted or parsed differently
    (b3c) => {
      const noun = b3c.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kara = b3c.particle('から', 'kara');
      const to = b3c.particle('と', 'to');
      const itte = b3c.tok({ textOneOf: ['いって', 'って', 'といって'] }, 'itte');

      b3c.inOrder(noun, kara, 3);
      b3c.inOrder(kara, to, 1);
      b3c.inOrder(to, itte, 1);

      b3c.captureSpan('からといって', noun, itte);
    },

    // Pattern 3d: Na-adjective/Noun + だからといって (single token)
    // Sometimes だからといって is parsed as one token
    (b3d) => {
      const noun = b3d.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dakaratoitte = b3d.tok({ text: 'だからといって' }, 'dakaratoitte');

      b3d.inOrder(noun, dakaratoitte, 5);
      b3d.captureSpan('からといって', noun, dakaratoitte);
    },

    // Pattern 4: Shortened form からって (colloquial)
    // e.g., 暑いからって、行くからって
    (b4) => {
      const verbOrAdj = b4.tok({ posOneOf: ['VERB', 'ADJ'] }, 'verbOrAdj');
      const karatte = b4.tok({ text: 'からって' }, 'karatte');

      b4.inOrder(verbOrAdj, karatte, 5);
      b4.captureSpan('からといって', verbOrAdj, karatte);
    },

    // Pattern 4b: Noun + だ + からって (colloquial)
    // e.g., 便利だからといって (but written as だからって in speech)
    (b4b) => {
      const noun = b4b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dakaratte = b4b.tok({ text: 'だからって' }, 'dakaratte');

      b4b.inOrder(noun, dakaratte, 5);
      b4b.captureSpan('からといって', noun, dakaratte);
    },

    // Pattern 5: Literary form からとて
    // e.g., 日本に2年間住んでいたからとて
    (b5) => {
      const verbOrAdj = b5.tok({ posOneOf: ['VERB', 'ADJ'] }, 'verbOrAdj');
      const karatote = b5.tok({ text: 'からとて' }, 'karatote');

      b5.inOrder(verbOrAdj, karatote, 5);
      b5.captureSpan('からといって', verbOrAdj, karatote);
    },

    // Pattern 5b: Noun + だ + からとて (literary)
    (b5b) => {
      const noun = b5b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dakaratote = b5b.tok({ text: 'だからとて' }, 'dakaratote');

      b5b.inOrder(noun, dakaratote, 5);
      b5b.captureSpan('からといって', noun, dakaratote);
    },

    // Pattern 6: Catch-all for split tokenization variations (verb/adj + だ/から + various forms)
    // Handles cases where tokens are split differently
    (b6) => {
      const predicate = b6.tok({ posOneOf: ['VERB', 'ADJ'] }, 'predicate');
      const kara = b6.tok({ textOneOf: ['から', 'だから'] }, 'kara');
      const toitte = b6.tok({ textOneOf: ['といって', 'からといって', 'からって', 'からとて'] }, 'toitte');

      b6.inOrder(predicate, kara, 5);
      b6.inOrder(kara, toitte, 2);

      b6.captureSpan('からといって', predicate, toitte);
    },

    // Pattern 7: Catch-all for split tokenization variations (verb/adj + だ/から + various forms)
    // Handles cases where tokens are split differently
    (b7) => {
      const predicate = b7.tok({ posOneOf: ['VERB', 'ADJ'] }, 'predicate');
      const kara = b7.tok({ textOneOf: ['から', 'だから'] }, 'kara');
      const toitte = b7.tok({ textOneOf: ['といって', 'からといって', 'からって', 'からとて'] }, 'toitte');

      b7.inOrder(predicate, kara, 5);
      b7.inOrder(kara, toitte, 2);

      b7.captureSpan('からといって', predicate, toitte);
    },

    // Pattern 8: Catch-all for noun patterns
    (b8) => {
      const noun = b8.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const kara = b8.tok({ textOneOf: ['だから', 'から'] }, 'kara');
      const toitte = b8.tok({ textOneOf: ['といって', 'からといって', 'からって', 'からとて'] }, 'toitte');

      b8.inOrder(noun, kara, 5);
      b8.inOrder(kara, toitte, 2);

      b8.captureSpan('からといって', noun, toitte);
    },

    // Pattern 9: Very loose catch-all - match any predicate + kara + to + any ending
    // This handles unexpected GiNZA tokenizations
    (b9) => {
      const predicate = b9.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON'] }, 'predicate');
      const kara = b9.particle('から', 'kara');
      const to = b9.tok({ text: 'と' }, 'to');
      const ending = b9.tok({ textOneOf: ['って', 'いって', 'いう'] }, 'ending');

      b9.inOrder(predicate, kara, 5);
      b9.inOrder(kara, to, 2);
      b9.inOrder(to, ending, 2);

      b9.captureSpan('からといって', predicate, ending);
    },

    // Pattern 10: Match predicate + kara + to + itte/itte (very flexible)
    // Handles various tokenizations of 〜からといって
    (b10) => {
      const predicate = b10.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON', 'AUX', 'ADV'] }, 'predicate');
      const kara = b10.particle('から', 'kara');
      const to = b10.tok({ textOneOf: ['と', 'って', 'いって'] }, 'to');
      const itte = b10.tok({ textOneOf: ['いって', 'って', 'いう'] }, 'itte');

      b10.inOrder(predicate, kara, 5);
      b10.inOrder(kara, to, 3);
      b10.inOrder(to, itte, 2);

      b10.captureSpan('からといって', predicate, itte);
    },

    // Pattern 11: Ultra-loose catch-all - match predicate + kara + ending token
    // This catches all remaining edge cases
    // NOTE: This may cause false positives on からする (karasuru) patterns
    (b11) => {
      const predicate = b11.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON', 'AUX', 'ADV', 'PART'] }, 'predicate');
      const kara = b11.particle('から', 'kara');
      const ending = b11.tok({ textOneOf: ['と', 'って', 'いって', 'といって', 'からといって', 'からって', 'からとて'] }, 'ending');

      b11.inOrder(predicate, kara, 5);
      b11.inOrder(kara, ending, 3);

      b11.captureSpan('からといって', predicate, ending);
    }
  );
});
