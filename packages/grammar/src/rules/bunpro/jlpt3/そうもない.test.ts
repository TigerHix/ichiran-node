import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そうもない.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // そうだ (positive conjecture, different meaning)
  '雨が降りそうです。',
  'この教科書はとてもむずかしそうです。',
  // そうにない (weaker negative conjecture, different particle)
  '雨が降りそうにない。',
  // Separate sou + mo + nai not connected
  'そう、もいない人だ。',

  // Hearsay そうだ (名詞-助動詞語幹, not 形状詞-助動詞語幹)
  // '彼は来るそうだ。', // This wouldn't match anyway because of structure
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
