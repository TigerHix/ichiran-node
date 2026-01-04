import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './っぽい.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ぽい as independent word (rare, different usage)
  // This is the suffix っぽい we're targeting, not standalone usage

  // Regular i-adjectives (different from -っぽい suffix)
  '白い色が好きです。',           // plain i-adjective "white"
  '黒い服を着ています。',         // plain i-adjective "black"
  '安い物は質が悪い。',           // plain i-adjective "cheap"

  // そう (looks like/seems) - different grammar
  '雨が降りそうです。',           // looks like it will rain
  '彼は悲しそうです。',           // he looks sad

  // らしい (typical of) - different grammar
  '彼は学生らしい。',             // typical of a student
  '子供らしい遊び。',             // childlike play (positive nuance)

  // がち (tendency to) - different grammar
  '遅刻しがちだ。',               // tends to be late (neutral)
  '病気がちの子供。',             // child prone to illness
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
