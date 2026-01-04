import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ところが.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ところ (noun: "place") - different grammar
  'いいところを見つけた。',
  'ここはいいところです。',
  '遊びに行くところです。',

  // ところで (topic changer: "by the way") - different grammar
  'ところで、時間はありますか。',

  // が (simple contrast) - different grammar
  '寿司が好きです。',
  '雨が降っています。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
