import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いよいよ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the いよいよ grammar rule
const negatives = [
  // いよ (iyo) - different word, not the adverb
  // "Iyo" as interjection or different contexts

  // Similar adverbs with different meanings
  // やっと (yatto) - "finally" with positive nuance after effort
  'やっと終わった。',
  'やっと家に着いた。',

  // ようやく (yoyaku) - "finally, at last" (more formal than yatto)
  'ようやく春が来た。',
  'ようやく試験が終わった。',

  // ついに (tsui ni) / 遂に - more formal "finally"
  'ついに完成した。',
  '遂に勝利した。',

  // とうとう (toutou) - "finally, after all"
  'とうとう雨が降ってきた。',
  'とうとう帰ってしまった。',

  // ますます (masumasu) - "more and more" (only increasing, no climax)
  'ますます強くなる。',
  'ますます寒くなってきた。',

  // Words starting with いよ but different
  // No common false positives here as いよいよ is a unique adverb

  // Contexts where similar forms might appear but aren't the grammar point
  // いよいよ as name or title (very rare, unlikely to occur naturally)
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
