import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たい.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // たがる (third-person desire) - different grammar point
  '彼は行きたがっている。',
  '子供はケーキを食べたがる。',

  // がほしい (want something) - different grammar point
  'お金がほしい。',
  '新しい車がほしい。',

  // てほしい (want someone to do something) - different grammar point
  '来てほしい。',
  '手伝ってほしい。',

  // Verbs ending with similar sounds but not たい auxiliary
  '持っている。',    // ～ている (progressive)
  'してしまった。',  // ～てしまう (completed action)
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
