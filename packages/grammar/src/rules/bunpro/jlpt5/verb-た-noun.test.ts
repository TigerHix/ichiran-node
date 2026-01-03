import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-た-noun.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Verb + た at end of sentence (not modifying a noun)
  '私は本を読んだ。',
  '彼が来た。',
  '昨日、ご飯を食べた。',

  // Adjective + noun (different grammar)
  'おいしいケーキ',
  '高い車',
  'きれいな服',

  // Verb in dictionary form + noun (different grammar)
  '食べる人',
  '行く犬',

  // Noun + だ (copula, not verb + た)
  '今日は休みだ。',
  '彼は学生だ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
