import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないほうがいい.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Positive advice form (たほうがいい - should do, not shouldn't)
  '行ったほうがいい。',
  '食べたほうがいいです。',
  '勉強したほうがいい。',

  // Simple negative form (～ない) - without ほうがいい
  '行かない。',
  '食べない。',
  'しない。',

  // ほうがいい comparing things (not advice)
  'このほうがいい。',
  'その方がいい。',
  '雨のほうがいい。',

  // ない as negation in other contexts
  'お金がない。',
  '彼は来ない。',

  // いい alone (good)
  'それはいい。',
  'いい天気です。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: はかないほう (hakanaifuu) - single tokenization issue
//
// GiNZA parses this inconsistently:
//   はかないほう → lemma=はかない, pos=ADV, dep=advmod ✗ INDISTINGUISHABLE
//
// The issue is that GiNZA tokenizes "はかないほう" as a single ADV token
// (lemma=はかない), rather than splitting it into verb stem + ない + ほう.
//
// Compare to working parses:
//   吸わない → lemma=吸う (VERB) + lemma=ない (AUX) ✓ WORKS
//   食べない → lemma=食べる (VERB) + lemma=ない (AUX) ✓ WORKS
//
// When "はかないほう" is tokenized as a single ADV token with lemma=はかない,
// it's indistinguishable from other adverbs like:
//   - おそらく (perhaps)
//   - もちろん (of course)
//   - はたして (really/actually)
//
// Matching all ADV tokens with lemma ending in ない would cause massive overcapture.
//
// CONCLUSION: GiNZA limitation - single tokenization prevents matching.
const skipPositives = [
  'この靴をはかないほうがいい。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, {
    negatives,
    skipPositives,
  });
});
