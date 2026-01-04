import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './うちに.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with うち that should NOT match the temporal pattern
const negatives = [
  // うち as "house/home" with additional case particles (not temporal)
  '私のうちには猫がいる。',
  'あなたのうちでパーティーをしました。',
  // うち with topic marker (not temporal)
  '暗いうちは怖い。',
  '外は寒いが、うちは暖かい。',
  // うち as part of counter phrase (not temporal)
  '三人のうちから一人を選ぶ。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
