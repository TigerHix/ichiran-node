import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './を経て.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // 経 kanji but different verb (経つ not 経る)
  '三年を経った後で連絡が来た。',
  // を + へ but へ is directional, not verb
  '荷物を手に持って東京へ向かった。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
