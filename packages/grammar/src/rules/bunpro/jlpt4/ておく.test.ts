import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ておく.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // おく as standalone verb meaning "to place/put"
  '本を机の上に置く。',
  'ここに座ってもいいですか？',
  'カバンを置いて。',
  // ておいて (te-form of おく, not auxiliary)
  'リモコンを置いて。'
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
