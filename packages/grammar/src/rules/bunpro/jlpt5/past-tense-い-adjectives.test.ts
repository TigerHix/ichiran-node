import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './past-tense-い-adjectives.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // Present tense い-adjectives
  '寒い。',
  '寒いです。',
  'たのしい。',
  'たのしいです。',

  // Verb past tense (different grammar)
  '食べた。',
  '行った。',
  '買いました。',

  // な-adjective past tense (different conjugation)
  '静かだった。',
  '静かでした。',
  'きれいだった。',
  '親切だった。',

  // いい in present tense (not past)
  'いいです。',
  'いい天気です。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
