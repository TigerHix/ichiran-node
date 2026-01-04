import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './にくい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that must be skipped from positive tests
const skipPositives: string[] = [];

// Negative test cases - sentences that should NOT match the にくい grammar rule
const negatives = [
  // がたい (gatai) - more formal, objective difficulty
  '信じがたい話だ。',
  '得がたい機会だ。',
  '耐えがたい暑さだ。',

  // づらい (zurai) - psychologically difficult, painful to do
  '言いづらいことを聞く。',
  'これは読みづらい。',
  'その話は聞きづらい。',
  'お前には本当に言いづらいけど、お前のギターを壊した。ごめん。',

  // やすい (yasui) - opposite meaning (easy to)
  'この本は読みやすい。',
  '住みやすい街だ。',
  '使いやすい道具だ。',

  // Independent verb (not auxiliary use)
  '憎い。', // nikui as "hateful"
  '彼が憎い。',

  // Similar sounding but different patterns
  '肉が煮えた。', // nie-ta (boiled) not ni-kui
  '肉に煮る。',   // ni-neru (boil in) not ni-kui
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
