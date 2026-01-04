import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どうしても.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT match
  // どうにも (used with negative, different nuance)
  'どうにもできない。',
  'どうにもならない。',

  // どうせ (anyhow, resignation - different nuance)
  'どうせ遅刻する。',
  'どうせ無理だ。',

  // どうやら (apparently, seems like - different meaning)
  'どうやら雨が降りそうだ。',
  'どうやら成功したらしい。',

  // いくら (no matter how - but needs ても to be complete grammar)
  'いくら勉強してもわからない。',
  'いくら待っても来ない。',

  // どんなに (no matter how - but needs ても)
  'どんなに頑張っても勝てない。',
  'どんなに忙しくても行く。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
