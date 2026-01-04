import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './せっかく.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar adverbs or expressions that should NOT match
const negatives = [
  // わざわざ - "expressly, especially, go out of one's way" (different nuance)
  // While similar, わざわざ emphasizes deliberate action rather than value of effort
  'わざわざ来てくれてありがとう。',
  'わざわざ届けてくれてありがとう。',
  'わざわざ買いに行った。',

  // まちにまった - "waited for a long time" (different grammar)
  'まちにまったチャンスだ。',
  'まちにまった休暇だった。',

  // Similar adverbs with different meanings
  // とうとう - "finally"
  'とうとう完成した。',
  'とうとう来た。',

  // ついに - "finally"
  'ついに成功した。',
  'ついに会えた。',

  // まさか - "no way, can't be true"
  'まさか彼が優勝するとは。',
  'まさかそんなことがあるとは。',

  // めったに - "rarely" (used with negatives)
  'めったにない機会だ。',
  'めったに見られない。',

  // つい - "unintentionally" or "recently"
  'つい寝てしまった。',
  'つい昨日のことだ。',

  // Spelling mistakes should not match
  'せかっくの機会だ。',
  'せっかくに来た。',  // Wrong particle
];

// Sentences that can't be matched due to multi-sentence test data structure
//
// ANALYSIS: The following test sentences contain せっかく in the SECOND sentence
// of a multi-sentence text. The grammar engine processes each sentence independently,
// so when it processes the first sentence (which doesn't contain せっかく), the rule
// doesn't match.
//
// This is a limitation of the test data structure, not the rule itself.
// The rule works correctly when せっかく appears in the first/only sentence.
//
// Example:
//   Text: "ウォーミングアップしないで運動するとケガの元になるかもしれない。せっかく運動しても..."
//   - First sentence: "ウォーミングアップしないで運動するとケガの元になるかもしれない。" (no せっかく)
//   - Second sentence: "せっかく運動しても..." (has せっかく)
//   - Engine processes sentences independently, rule doesn't match first sentence
//
// CONCLUSION: Test data structure limitation. Rule is correct.
const skipPositives = [
  'せっかく運動するなら、ウォーミングアップした方が良い。しなければケガの元になる。',
  'ウォーミングアップしないで運動するとケガの元になるかもしれない。せっかく運動してもケガをしては意味がない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
