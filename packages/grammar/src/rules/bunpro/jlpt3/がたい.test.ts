import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がたい.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative test cases - sentences that should NOT match the がたい grammar rule
// These test similar suffixes that have different meanings
const negatives = [
  // にくい (nikui) - different from がたい, less formal, subjective difficulty
  '彼の声は聞こえにくい。',
  'この薬は飲みにくい。',
  'この辞書は使いにくい。',

  // づらい (zurai) - physically difficult to do
  'これは食べづらい。',
  'その靴は歩きづらい。',
  '読みづらい字だ。',

  // がち (gachi) - tendency to do something (different grammar)
  '彼は遅れがちだ。',
  '雨が降りがちです。',

  // Independent verb (not auxiliary use)
  '難しい。',
  '困難だ。',

  // Similar sounding but different patterns
  '重い荷物だ。', // omoi (heavy) not gatai
  '固い約束だ。', // katai (hard/firm) not gatai
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
