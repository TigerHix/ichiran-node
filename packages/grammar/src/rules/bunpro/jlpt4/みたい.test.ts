import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './みたい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: Similar patterns that should NOT match
const negatives = [
  // ようだ (formal version - different grammar)
  '春のようだ。',
  '雨が降るようです。',

  // ように・ような (formal adverbial forms - different grammar)
  '春のように暖かい。',
  '春のような天気です。',

  // にみえる (visual appearance - different grammar)
  '彼は老人に見える。',
  'このお菓子はおいしそうに見える。',

  // そう (appearance-based conjecture - different grammar)
  '雨が降りそうです。',
  'このケーキはおいしそうです。',

  // らしい (hearsay/typical - different grammar)
  '彼は日本人らしい。',
  '雨が降るらしい。',

  // みたいだ (hearsay - different grammar point)
  // Actually, みたい can be used for hearsay, but that's not this grammar point
  // This grammar point is specifically for "looks like/seems like"
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
