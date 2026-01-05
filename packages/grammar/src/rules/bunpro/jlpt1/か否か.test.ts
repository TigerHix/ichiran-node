import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './か否か.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Simple かどうか (different grammar point - informal version)
  '行くかどうかわからない。',
  'できるかどうか試してみる。',
  // Negative sentences ending in 否, not か否か pattern
  '彼は否と答えた。',
  '是非に及ばず。',
  // か...か pattern without 否
  '行くか行かないか迷っている。',
  '来るか来ないかまだ決めていない。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
