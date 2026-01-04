import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './か何か.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Simple question with just か (not followed by 何か)
  '行きますか。',
  '食べるか。',
  'これは本ですか。',
  // 何か without the preceding か particle
  '何か食べる？',
  '何かあったの？',
  // Separate instances of か and 何か
  'これは何か。か？',
  // か as a particle marking a quote or list item (without 何か)
  'リンゴかバナナかを選ぶ。',
  // かどうか (whether or not) - different grammar pattern
  '行くかどうかわからない。',
];

// Positive tests to skip due to GiNZA tokenization limitations
const skipPositives = [
  // GiNZA splits "昨日食べた肉かなにかが..." at the period, so "か" and "なにか" are in different sentences
  '「彼は体調が悪くて寝ている。昨日食べた肉かなにかがよくなかったみたい。」',
  // Same issue - "かなにか" split across sentences
  '友達１：「どうしたの？まるで幽霊かなにかを見たような青白い顔をしているよ！」幸運に恵まれている友達２：「あのね、たった今宝くじが当たったの。１００万ドルをもらうのよ！」',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
