import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ように-てほしい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match ように～てほしい
const negatives = [
  // てほしい without ように (different grammar point)
  '勉強してほしい。',
  '来てほしいです。',

  // ように without てほしい (different grammar point)
  '日本語のように話す。',
  '大人のようになる。',

  // Simple ようだ (seems like) - different grammar
  '彼は学生のようだ。',
  '雨が降るようです。',

  // たい (want to do oneself, not want someone else)
  '日本語を話せるようになりたい。',
  '大人のようになりたいです。',

  // がほしい (want something, not want someone to do)
  '新しいのがほしい。',
  'お金がほしくないです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
