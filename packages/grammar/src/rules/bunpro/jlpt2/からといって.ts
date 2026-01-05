import { linguisticRule } from '../../../engine/lang.js';

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
 * - いって as VERB or AUX
 * - Often has compound, fixed, or mark dependencies
 *
 * Different from:
 * - から alone (because/from)
 * - と言って (called/say - quotative)
 * - からして (judging from)
 * - からすると (more objective judgment)
 */
export default linguisticRule('からといって', (r) => {
  r.either(
    // Pattern 1: Verb + からといって (full form)
    // e.g., 慰められたからといって、したからといって、行ったからといって
    (b1) => {
      const verb = b1.verb({}, 'verb');
      const kara = b1.particle('から', 'kara');
      const to = b1.particle('と', 'to');
      const itte = b1.tok({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      b1.inOrder(verb, kara, 5);
      b1.inOrder(kara, to, 1);
      b1.inOrder(to, itte, 1);

      b1.captureSpan('からといって', verb, itte);
    },

    // Pattern 1b: Verb + から + といって (single token)
    // Sometimes といって is parsed as a single token
    (b1b) => {
      const verb = b1b.verb({}, 'verb');
      const kara = b1b.particle('から', 'kara');
      const toitte = b1b.tok({ text: 'といって' }, 'toitte');

      b1b.inOrder(verb, kara, 5);
      b1b.inOrder(kara, toitte, 1);

      b1b.captureSpan('からといって', verb, toitte);
    },

    // Pattern 1c: Verb + からといって (single combined token)
    // Sometimes the entire pattern is one token
    (b1c) => {
      const verb = b1c.verb({}, 'verb');
      const karatoitte = b1c.tok({ text: 'からといって' }, 'karatoitte');

      b1c.inOrder(verb, karatoitte, 5);
      b1c.captureSpan('からといって', verb, karatoitte);
    },

    // Pattern 2: I-adjective + からといって (full form)
    // e.g., 貧しいからといって、安いからといって、懐かしいからといって
    (b2) => {
      const adj = b2.adj({}, 'adj');
      const kara = b2.particle('から', 'kara');
      const to = b2.particle('と', 'to');
      const itte = b2.tok({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      b2.inOrder(adj, kara, 5);
      b2.inOrder(kara, to, 1);
      b2.inOrder(to, itte, 1);

      b2.captureSpan('からといって', adj, itte);
    },

    // Pattern 2b: I-adjective + からといって (single token)
    (b2b) => {
      const adj = b2b.adj({}, 'adj');
      const karatoitte = b2b.tok({ text: 'からといって' }, 'karatoitte');

      b2b.inOrder(adj, karatoitte, 5);
      b2b.captureSpan('からといって', adj, karatoitte);
    },

    // Pattern 3: Na-adjective/Noun + だ + からといって (full form)
    // e.g., 生意気だからといって、丈夫だからといって、日本人だからといって
    (b3) => {
      const noun = b3.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const da = b3.aux({ lemma: 'だ' }, 'da');
      const kara = b3.particle('から', 'kara');
      const to = b3.particle('と', 'to');
      const itte = b3.tok({ lemma: '言う', inflectionForm: '連用形-一般' }, 'itte');

      b3.inOrder(noun, da, 2);
      b3.inOrder(da, kara, 2);
      b3.inOrder(kara, to, 1);
      b3.inOrder(to, itte, 1);

      b3.captureSpan('からといって', noun, itte);
    },

    // Pattern 3b: Na-adjective/Noun + だからといって (single token)
    // Sometimes だからといって is parsed as one token
    (b3b) => {
      const noun = b3b.tok({ posOneOf: ['NOUN', 'PROPN', 'PRON'] }, 'noun');
      const dakaratoitte = b3b.tok({ text: 'だからといって' }, 'dakaratoitte');

      b3b.inOrder(noun, dakaratoitte, 5);
      b3b.captureSpan('からといって', noun, dakaratoitte);
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

    // Pattern 6: Catch-all for split tokenization variations
    // Handles cases where tokens are split differently
    (b6) => {
      const predicate = b6.tok({ posOneOf: ['VERB', 'ADJ', 'NOUN', 'PROPN', 'PRON'] }, 'predicate');
      const kara = b6.tok({ textOneOf: ['から', 'だから'] }, 'kara');
      const toitte = b6.tok({ textOneOf: ['といって', 'からといって', 'からって', 'からとて'] }, 'toitte');

      b6.inOrder(predicate, kara, 5);
      b6.inOrder(kara, toitte, 2);

      b6.captureSpan('からといって', predicate, toitte);
    }
  );
});
