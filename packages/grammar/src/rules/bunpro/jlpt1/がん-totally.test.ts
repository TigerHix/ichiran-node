import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がん-totally.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative examples: sentences that should NOT match
const negatives = [
  // Just "見る" without ガン
  '彼を見た。',
  '彼女を見ていた。',

  // Just "無視" without ガン
  '彼を無視した。',
  '無視されて悲しかった。',

  // 頑張る (ganbaru) - different word, not the ガン prefix
  '頑張ってください。',
  '彼は頑張っている。',

  // 頑固 (ganko) - stubborn, different word
  '彼は頑固だ。',

  // 銀 (gin) - silver, different word
  '銀行に行った。',
  '銀座で買い物をした。',

  // がん (cancer) - literal meaning
  'がんの治療を受けている。',
  '彼はがんになった。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
