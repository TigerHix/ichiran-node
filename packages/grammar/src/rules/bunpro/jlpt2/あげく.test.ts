import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あげく.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar "after" expressions that are NOT あげく
  // あと - "after" (general temporal marker, not necessarily negative)
  '映画を見たあと、食事をしました。',
  '仕事のあとで、飲みに行きましょう。',

  // うえで - "after doing" (focuses on process/sequence)
  '詳しく調べたうえで、決めます。',
  'よく相談したうえで、返事します。',

  // すえ - "after" (emphasizes long process)
  '長い議論のすえ、結論が出た。',
  '苦労のすえ、成功した。',

  // のすえ - variant of すえ
  '長期間の努力のすえ、完成した。',

  // うえ - "in addition" or "on top of" (different meaning)
  '疲れたうえに、眠い。',
  '値段が高いうえに、質が悪い。',

  // Sentences with あげく in different contexts (if they exist)
  // e.g., あげく as a standalone noun not following verb-ta or noun-no
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
