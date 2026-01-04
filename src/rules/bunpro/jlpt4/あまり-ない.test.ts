import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あまり-ない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // あまり at end of sentence as noun (remainder/surplus)
  'まだ時間があまりある。',
  'お金があまり残っている。',

  // あまり with positive verb/adjective
  'このケーキはあまり美味しい。',

  // Positive emphasis usage (different grammar point)
  // Note: "驚きのあまり声も出なかった" has same surface form as あまり-ない
  // but means "so much that" instead of "not very". These patterns are
  // contextually ambiguous and difficult to distinguish purely syntactically.
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// 1. 最近はどこもあまり平和ではない。
//    - "平和" (peace) is tagged as '形容詞-一般' instead of expected noun/na-adj tag
//    - The rule expects tags: ['名詞-普通名詞-一般', '形状詞-一般']
//    - GiNZA incorrectly tags "平和" as an adjective in this context
const skipPositives = [
  '最近はどこもあまり平和ではない。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
