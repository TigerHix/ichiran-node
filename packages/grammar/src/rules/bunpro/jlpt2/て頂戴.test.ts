import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './て頂戴.js';
import { BUNPRO_JLPT2 } from './index.js';

// No GiNZA parsing issues identified yet - all test sentences should match

// Negative test cases - sentences that should NOT match the て頂戴 grammar rule
const negatives = [
  // てください (te kudasai) - polite request (different grammar)
  '開けてください。',
  '座ってください。',
  '勉強してください。',
  '入ってください。',

  // て (te-form) without choudai - different grammar
  '開けて。',
  '座って。',
  '勉強して。',

  // なさい (nasai) - "please do" (different grammar)
  '座りなさい。',
  '勉強しなさい。',
  '入りなさい。',

  // Standard imperatives (命令形) - different grammar
  '食べろ！',
  '座れ！',
  '来い！',

  // Similar patterns without the full grammar
  '開ける。',
  '買う。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
