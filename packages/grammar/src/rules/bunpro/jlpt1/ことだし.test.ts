import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことだし.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Simple だし listing (without こと) - different grammar
  '雨だし寒いし、出かけたくない。',
  // から or ので instead of ことだし
  '天気がいいから、ピクニックに行きましょう。',
  'お金がないので、買えません。',
  // こと as regular noun (not nominalizer)
  '彼女のことが好きです。',
  // ことだ but without し (different grammar)
  '一番大切なことは努力することだ。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
