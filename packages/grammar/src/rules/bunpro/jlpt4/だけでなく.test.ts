import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけでなく.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // だけで (only with) without なく - no "not only" meaning
  '三日だけで完成した。',
  '一人だけで行く。',
  // だけ + ない separately (simple negation of "only")
  '彼だけない。',
  // でなく without だけ (not X but Y)
  '問題は彼でなく私だ。',
  // だけ and なく in separate clauses
  'これだけ食べたら、もう食べなくていい。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
