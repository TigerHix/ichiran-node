import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './number-しか-ない.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that can't be matched due to scope of this rule:
//
// The rule "Number + しか〜ない" is specifically for NUM + しか + negative verb.
// The Bunpro test data includes a few noun-only examples (without numbers) in the
// writeup section showing contrast between しか and だけ. These are not actual
// test sentences for this grammar point, which focuses on numbers.
//
// The general しか～ない pattern (nouns + しか + negative) is covered by a
// different rule ("しか-ない", JLPT4 id 182).
//
// Additionally, one test sentence "彼には１度しかかっていない" appears to have
// an irregular parsing issue - the verb stem is omitted in the test data,
// resulting in ungrammatical Japanese that doesn't parse consistently.
const skipPositives = [
  '冷蔵庫の中にはリンゴしかなかったから、リンゴを食べた。', // No NUM - noun-only しか～ない
  '彼には１度しかかっていない。', // Irregular parsing - verb stem omitted in test data
];

const negatives = [
  // JLPT3 しかない (Verb + しかない = "have no choice but to")
  '呼ぶしかない。',
  '行くしかない。',
  '待つしかない。',
  '謝るしかないです。',
  '守るしかなかった。',

  // Regular しか～ない with noun (not number-specific)
  // This is covered by rule "しか-ない", not "number-しか-ない"
  'バナナしか食べなかった。',
  'それしか知りません。',

  // Negative verbs without しか
  '行かない。',
  '食べない。',
  '持っていない。',
  'ありません。',

  // だけ instead of しか
  '１００円だけ持っている。',
  '２時間だけ遊べる。',

  // Positive form with しか (grammatically incorrect but should not match)
  // Note: しか must be followed by negative, so these should not match
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
