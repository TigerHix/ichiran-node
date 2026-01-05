import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ざる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ざる grammar rule
const negatives = [
  // ず (zu) - terminal form, not attributive
  // Same auxiliary but different form (used to end clauses)
  '勉強せずに済んだ。',
  '彼は来ずに帰った。',
  '雨が降らずに終わった。',

  // ぬ (nu) - another classical negative form
  // Similar meaning but different surface form
  '行かぬこともある。',
  '知らぬふりをする。',
  '来ぬ間に桜が散った。',

  // ない (nai) - modern negation
  // Different grammar, modern form
  '知らない人。',
  '行かないでください。',
  '食べないことがある。',

  // ざるを得ない (zaru o enai) - different grammar
  // Related expression meaning "have no choice but to"

  // Similar sounding words
  // 座る (suwaru) - to sit
  '座ってください。',

  // さる (saru) - monkey
  '猿が木に登っている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
