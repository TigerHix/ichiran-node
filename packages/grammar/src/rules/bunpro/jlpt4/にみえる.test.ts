import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './にみえる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // みえる alone - potential form of 見る (can see), not appearance
  '富士山が見える。',
  '海が見える。',
  // がみえる - potential form with subject marker
  '彼が見える。',
  // とみえる - objective conclusion (different grammar point)
  '暑いとみえて、汗をかいている。',
  '留守とみえた。',
  // そうです - hearsay, not appearance
  '彼は来るそうです。',
  '明日は雨だそうです。',
  // そう alone (without に)
  'そう思う。',
  'そうですね。',
  // らしい - hearsay or general impression (not specifically visual)
  '彼は日本人らしい。',
  '明日は雨らしい。',
  // ようだ - subjective conjecture (not specifically visual appearance)
  '彼は日本人のようだ。',
  // みたい - casual similarity/likelihood (not specifically visual)
  '彼は日本人みたい。',
  // Adjective ending in さ + そう (different pattern)
  '大きそうだ。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
