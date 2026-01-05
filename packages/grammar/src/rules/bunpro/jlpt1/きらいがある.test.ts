import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './きらいがある.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Different expression: 傾向がある (neutral tendency)
  '日本人は魚を食べる傾向がある。',
  // Different expression: がち (tends to, attaches to verb stem)
  '彼は忘れがちだ。',
  // きらい as adjective meaning "dislike" (not the grammar pattern)
  '私は彼がきらいだ。',
  // きらい as noun meaning "dislike" with different verb
  'このきらいを直さなければならない。',
  // ある as "have/possess" (different meaning)
  '彼はきらいな食べ物がある。',
  // Sentence-final きらい (incomplete)
  '彼の意見にはきらいが',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
