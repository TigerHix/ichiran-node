import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なかなか-ない.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // なかなか (positive usage, different grammar)
  'なかなか面白い映画だった。',
  'なかなかいい天気ですね。',
  'なかなか美味しいですね。',

  // あまり-ない (JLPT4) - "not very, not much"
  'あまり食べない。',
  'あまり美味しくない。',

  // 全く-ない (JLPT3) - "not at all"
  '全くできない。',
  '全くわからない。',

  // あまり (JLPT3) - "so much that"
  '驚きのあまり声も出なかった。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
