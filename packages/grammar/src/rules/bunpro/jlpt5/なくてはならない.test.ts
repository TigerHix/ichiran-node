import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なくてはならない.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Simple negative verb (not te-form)
  '行かない。',
  '食べない。',
  'しない。',

  // Negative te-form + iru (state)
  '知らなくている。',
  '行かなくている。',

  // te-form + wa + naranai BUT positive form (prohibition, not obligation)
  '行ってはならない。',
  '食べてはならない。',
  '入ってはならない。',

  // Similar obligation forms (different grammar - handled by different rules)
  // なければならない (JLPT4)
  '行かなければならない。',
  '勉強しなければならない。',

  // なくてはいけない (similar but different auxiliary - different grammar rule)
  '行かなくてはいけない。',
  '勉強しなくてはいけない。',

  // なくてはダメ (casual form with dame - different grammar)
  '行かなくてはダメ。',
  '食べなくてはダメ。',

  // Other te-form constructions
  '行かなくて、勉強する。',
  '食べなくて、寝た。',

  // Simple naranai (become not) without obligation meaning
  '彼は来なくならない。',
  '温度が上がらなくならない。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
