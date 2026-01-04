import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ばあいは.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: should NOT match as 場合は grammar
const negatives = [
  // 場合 used as standalone noun meaning "situation/case" without conditional meaning
  // This is difficult to distinguish without context, so we focus on clear negatives
  // Similar expressions that should not match:
  // "とき" (when) sentences - different grammar point
  '私が行くときは、早く行きます。',
  '雨が降ったときは、傘をさします。',
  // "なら" conditional - different grammar point
  '東京に行くなら、新幹線がいいです。',
  '安いなら買います。',
  // "にとって" (for/to) - different grammar point when not expressing "in the case of"
  '私にとって、これは大切です。',
  '子供にとって、これは難しいです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
