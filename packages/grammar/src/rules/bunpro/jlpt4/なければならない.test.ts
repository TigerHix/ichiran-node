import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なければならない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // なければいけない (different auxiliary verb - different grammar rule)
  'しなければいけない。',
  '行かなければいけない。',
  '勉強しなければいけません。',

  // なくてはいけない (te-form construction - different grammar rule)
  '行かなくてはいけない。',
  '勉強しなくてはいけない。',

  // なくてはならない (te-form construction - different grammar rule)
  '行かなくてはならない。',
  '勉強しなくてはならない。',

  // Simple conditional form without obligation
  '行かなければ、行ける。',
  'しなければ、大丈夫だ。',

  // Simple naranai (become not) without obligation meaning
  '彼は来なくならない。',
  '温度が上がらなくならない。',

  // Positive conditional + naranai (prohibition form)
  '行ってはならない。',
  '食べてはならない。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
