import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ようと思う-おうと思う.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Simple volitional without と思う
  '勉強しよう。',
  '一緒に行こう。',
  '食べよう。',
  // ようとする (attempt to do) - different grammar
  '彼は逃げようとする。',
  'ドアを開けようとする。',
  // ようとしている (in the process of attempting)
  '彼女は立ち上がろうとしている。',
  // ようとした (attempted but didn't complete)
  '彼は逃げようとした。',
  // Simple とおもう without volitional (just "I think that...")
  '明日は雨が降ると思う。',
  '彼は来ると思う。',
  // ようにする (make an effort to / make sure that)
  '毎日運動するようにしている。',
  '遅刻しないようにしている。',
  // ようになる (come to be / gradually change)
  '日本語が話せるようになりました。',
  // Separate thoughts, not connected volitional + 思う
  '勉強しよう。そう思う。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
