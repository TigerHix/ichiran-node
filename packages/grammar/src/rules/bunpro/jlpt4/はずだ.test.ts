import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './はずだ.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // はず as independent noun meaning "expectation" (not followed by copula)
  // These are ambiguous and could be either the grammar point or just the noun
  // The rule only matches when followed by copula (だ/です/だった) or standalone
  // Standalone はず at end of sentence is captured, but internal uses are not

  // Similar expressions that should NOT match
  // はずがない (should not/be unlikely to) - different grammar
  '彼が来るはずがない。',
  'そんなことがあるはずがない。',

  // はずもない (no way) - different grammar
  '失敗するはずもない。',

  // わけではない (not necessarily the case) - different grammar
  '彼が来ないわけではない。',

  // ものだ (general rule) - different grammar
  '人は誰でも間違いをするものだ。',

  // べきだ (should/ought) - different grammar
  'もっと勉強するべきだ。',
  '嘘をつくべきではない。',

  // つもりだ (intend to) - different grammar
  '明日は出発するつもりだ。',

  // ようだ (seems like) - different grammar
  '雨が降るようだ。',

  // だろう (probably) - different grammar
  '明日は晴れるだろう。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
