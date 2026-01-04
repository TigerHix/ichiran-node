import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のがすき.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // きらい (dislike) - different grammar point
  '彼は勉強がきらいだ。',
  '納豆がきらいです。',
  // が without preceding verb + の
  '私が好きです。',
  '彼が好きな人',
  // のがじょうず (good at) - different grammar point
  '彼は料理をするのがじょうずだ。',
  // のがへた (bad at) - different grammar point
  '私は歌を歌うのがへたです。',
  // Just 好き without verb + の + が
  '私はすしが好きです。',
  'リンゴが好きだ。',
  // こと nominalizer (different grammar point)
  '本を読むことが好きだ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
