import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いがい.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: 意外 (surprise/unexpected) should NOT match
const negatives = [
  // 意外 as ADJ meaning "surprising" or "unexpected"
  'たかし君もアニメ好きなの？ちょっと意外だね。',
  '俺こう見えても意外に肉を食べないの。',
  'これは意外な結果です。',
  '意外なことで驚きました。',
  '意外と簡単だった。',
  // 外 used as noun meaning "outside" (not 以外)
  '家の外で遊びます。',
  '外を見てください。',
  '外は寒いです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
