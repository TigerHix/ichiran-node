import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がある.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // がいる for animate objects (different grammar pattern)
  '猫がいる。',
  '犬がいる。',
  '先生がいる。',
  // が with other verbs (not ある)
  '私が食べる。',
  '彼が来た。',
  '雨が降る。',
  // Verb ending in ある but not as existence verb
  '残ってある。',
  '置いてある。',
  // Copula + ある (different structure)
  '幸せであれば。',
  // Topic marker + ある (not subject marker)
  '本はある。',
  '机はある。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
