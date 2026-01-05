import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それまでだ.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Just それまで without だ
  'それまで進んだ。',
  'まだそれまでではない。',
  // Noun + までだ (different grammar - "just X" or "only need to X")
  'やるまでだ。',
  '待つまでだ。',
  // それでも instead of それまで
  '失敗したらそれでもいい。',
  // 違う instead of だ
  'あきらめてしまったらそれは違う。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
