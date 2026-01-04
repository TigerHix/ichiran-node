import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './もし.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // たとえ (even if - different grammar, contrastive)
  'たとえ雨が降っても行きます。',
  'たとえ高くても買います。',

  // ただし (however - different word)
  '雨が降る。ただし、夕方からです。',

  // または (or - different word)
  'バスまたは電車で行きます。',

  // あるいは (or/possibly - different word)
  '明日あるいは明後日行きます。',

  // も + particle (emphasis particle "also/even", not conditional marker "if")
  '雨も降っている。',
  '時間もある。',
  '誰も来ません。',

  // もう (already - different word)
  'もう雨が降っている。',
  'もう行かなければならない。',

  // どんどん (rapidly - similar sound, different meaning)
  'どんどん雨が降ってきた。',

  // だんだん (gradually - similar sound, different meaning)
  'だんだん寒くなってきました。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
