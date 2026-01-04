import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いらっしゃる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Regular いる (to be) - not honorific
  '先生は教室にいる。',
  '犬がいる。',
  '田中さんは家にいる。',
  '彼はいない。',

  // Regular くる (to come) - not honorific
  '田中さんが来ました。',
  '友達が来る。',

  // Regular いく (to go) - not honorific
  '私は東京へ行く。',
  '先生が行きました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
