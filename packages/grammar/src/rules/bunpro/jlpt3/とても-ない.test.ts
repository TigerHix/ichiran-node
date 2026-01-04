import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とても-ない.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // とても-ない (JLPT3) - "not at all, cannot possibly"
  // This should NOT match:
  // 1. とても used positively (very/very much)
  'とても良い天気です。',
  'とても嬉しいです。',
  'とても美味しかった。',

  // 2. あまり-ない (JLPT4) - "not very, not much" (different grammar)
  'あまり食べない。',
  'あまり美味しくない。',

  // 3. 全く-ない (JLPT3) - different negative emphasis
  '全然できない。',
  '全くわからない。',

  // 4. すこしも-ない (JLPT4) - "not even a little"
  '少しも面白くない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
