import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てはいけない.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // te-form + iru (progressive aspect) - different grammar
  '食べている。',
  '行っている。',
  '待っている。',

  // te-form + shimatta (completed action with regret) - different grammar
  '行ってしまった。',
  '忘れてしまった。',
  '壊れてしまった。',

  // te-form + mo ii (permission) - different grammar
  '行ってもいい。',
  '食べてもいい。',
  '来てもいい。',

  // te-form + wa + naranai (similar prohibition pattern, different auxiliary)
  '勉強してはならない。',
  '使ってはならない。',
  '入ってはならない。',

  // te-form + wa + dame (more casual prohibition, different grammar)
  '行ってはだめ。',
  '食べてはだめ。',
  '来てはだめ。',

  // te-form + wa +ikenai BUT without proper structure (separate clauses)
  '行って、はいけないと言った。',

  // Other te-form constructions
  '本を読んで勉強する。',
  'ご飯を食べて寝た。',

  // Potential form without te-form
  'これはいけない。',

  // Similar but not prohibition
  '行ってはいける。', // "can go by going" (different meaning)
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
