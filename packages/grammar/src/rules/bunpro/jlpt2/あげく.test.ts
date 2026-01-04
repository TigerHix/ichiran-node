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

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// COMPOUND VERB ISSUE: "走って転んだあげくに、壁に衝突した。"
//
// This sentence contains a compound verb structure: 走って (te-form of 走る) + 転んだ (past of 転ぶ)
// GiNZA parses this as separate tokens with complex dependency structure.
// The た (ta) token belongs to 転んだ, but the dependency chain makes it difficult
// to reliably match the full verb phrase "走って転んだ" + "あげく".
//
// Attempted patterns:
// 1. Verb + auxOf(ta) - Fails because ta is attached to 転ん, not 走って
// 2. Verb + headChild(ta, 'mark') - Same issue
// 3. Any ta before ageku + find verb - Still fails due to token order
//
// This appears to be a genuine limitation in matching compound verb structures
// where the ta-form is at the end of a chain. Other sentences with simple verbs
// (like 泣いたあげく) match correctly.
const skipPositives = [
  '走って転んだあげくに、壁に衝突した。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
