import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いかん-ず.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negatives: similar patterns that should NOT match
const negatives = [
  // にもかかわらず (different grammar: "despite")
  '雨にもかかわらず、彼は来ました。',
  '努力したにもかかわらず、失敗しました。',

  // いかん as standalone (not part of this grammar pattern)
  '方法のいかんを考える必要がある。',

  // Similar but missing components
  '結果によって変化します。',  // によって instead of いかん+によらず
  '理由にかかわらず認めます。',  // missing いかん

  // Different verb ending
  '天候いかんに予定を立てる。',  // not かかわらず/よらず/とわず
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
