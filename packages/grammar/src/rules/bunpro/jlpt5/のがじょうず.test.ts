import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のがじょうず.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // のがへた (bad at) - different grammar
  '彼は絵を描くのが下手だ。',
  '私は料理をするのが下手です。',
  // Similar patterns with different particles
  '歌うのを練習する。',
  '歌うをするのが好き。',
  // 上手 used without noun clause marker
  '彼は上手だ。',
  '上手に書く。',
  // のがとくい (confident at) - different grammar
  '彼は日本語を話すのが得意です。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
