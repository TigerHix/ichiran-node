import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './げ.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  '彼は悲しそうです。',
  '雨が降りそうです。',
  '彼は学生らしい。',
  '彼は子供っぽい。',
  '山の深さがすごい。',
  '少し疲れ気味です。',
  '気をつけてください。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
