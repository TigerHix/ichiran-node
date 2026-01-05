import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あっての.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Simple te-form of ある without の
  'お金があって、幸せだ。',
  // ある in dictionary form + の
  'お金があるの',
  // Different verb + て + の
  '行っての成果', // if this even exists
  // あって as separate tokens not forming the pattern
  '会っての場所',
  // であって (copula te-form) not あって (ある te-form)
  '金貨であっての幸せ',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// COMPOUND VERB ISSUE:
//
// GiNZA parses some verb+ある compounds as single tokens:
//   支えあっての → 支えあっ (lemma=支えあう, compound verb) ✗ CANNOT MATCH
//   部員あっての  → 部員 (NOUN) + あっ (lemma=ある) ✓ WORKS
//
// The rule requires lemma='ある' to match the あって pattern.
// When GiNZA analyzes a compound like 支えあう as a single token,
// the lemma is 支えあう, not ある, making it indistinguishable.
//
// Matching all 連用形-促音便 verbs would cause massive overcapture on
// unrelated te-forms (待って, 書って, etc.).
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  'ベストセラー作者の演説：「 私の本は、家族の支えあってのものです。息子と夫がいなければここまでやってこれなかったと思います。」',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives, skipPositives });
});
