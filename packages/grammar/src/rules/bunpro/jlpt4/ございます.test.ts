import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ございます.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Regular ある (to be) - not polite
  '時間があります。',
  '水がある。',
  '駅があちらにある。',
  'トイレはこちらにある。',
  '部屋にある。',

  // Regular あります (polite form of ある) - not very polite
  '時間があります。',
  '水があります。',
  '駅があちらにあります。',
  'トイレはこちらにあります。',
  '部屋にあります。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
