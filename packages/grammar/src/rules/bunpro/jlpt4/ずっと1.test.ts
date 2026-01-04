import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずっと1.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar-sounding expressions that are not ずっと (continuously)
  // ずつ means "per" or "each" (suffix attached to quantities)
  '一個ずつ食べる。',
  '少しずつ進んでいる。',
  '三人ずつグループになる。',
  // Other adverbs that should not match
  'もっと速く走りたい。',
  'すっと立ち上がった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
