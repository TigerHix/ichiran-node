import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いよいよ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples: similar adverbs that should NOT match
const negatives = [
  // ついに - "finally" (more formal than いよいよ)
  'ついに完成した。',
  'ついに会えた。',
  'ついに大学を卒業する。',

  // とうとう - "finally, after all" (tends to have positive nuance)
  'とうとう完成した。',
  'とうとう来た。',
  'とうとう大学に入った。',

  // やっと - "finally, at last" (positive nuance, relief)
  'やっと終わった。',
  'やっと休める。',
  'やっと成功した。',

  // ようやく - "finally, at last" (positive nuance)
  'ようやく春が来た。',
  'ようやく完成した。',
  'ようやく理解できた。',

  // ますます - "more and more, increasingly" (purely progressive)
  'ますます寒くなってきた。',
  'ますます興味深くなる。',
  'ますます盛んになる。',

  // だんだん - "gradually, little by little"
  'だんだん寒くなってきた。',
  'だんだん慣れてきた。',
  'だんだん分かってきた。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
