import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ている1.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Simple te-form without いる (different grammar - verb-て)
  '本を読んで寝た。',
  'ご飯を食べて学校に行きます。',
  // Past tense (た-form, not て-form)
  '本を読んだ。',
  'ご飯を食べた。',
  // Te-form requests (てください - different grammar)
  '本を読んでください。',
  'ドアを開けてください。',
  // Potential form (て-form + いる but different meaning)
  // Note: These are actually different meanings, so should be separate
  // Negative forms (handled by ていない pattern, not ている)
  // '勉強していない。',  // This might match if we're not careful
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
