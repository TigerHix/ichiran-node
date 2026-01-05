import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そうにない.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // そうだ (positive conjecture, different meaning)
  '雨が降りそうです。',
  'この教科書はとてもむずかしそうです。',
  // そうもない (stronger negative conjecture, different particle)
  '雨が降りそうもない。',
  'できそうもない。',
  // Separate sou + ni + nai not connected
  'そう、にはいけない。',
  // Hearsay そうだ (名詞-助動詞語幹, not 形状詞-助動詞語幹)
  '彼は来るそうだ。',
  // そう + に used as "in that way/manner" (adverbial usage)
  'そうに見える。',
  'そうに思う。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
