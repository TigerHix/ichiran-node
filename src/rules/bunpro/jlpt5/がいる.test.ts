import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がいる.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // ある for inanimate objects (not いる)
  '机がある。',
  '車がある。',
  '本がある。',
  // が with other verbs (not いる)
  '私が食べる。',
  '彼が来た。',
  '猫が走る。',
  // がある (inanimate)
  '猫がある。',
  '犬がある。',
  // Verb ending in いる but not as existence verb
  '行っている。',
  '知っている。',
  // Copula + いる (different structure)
  '先生でいる。',
  // Noun modifier + いる (possession, not existence)
  '私のいる場所。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
