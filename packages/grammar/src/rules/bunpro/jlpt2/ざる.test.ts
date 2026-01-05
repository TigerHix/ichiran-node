import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ざる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ざる grammar rule
// These test modern negative forms and related grammar patterns
const negatives = [
  // Modern ない negative forms
  '知らない人。',
  '食べないでください。',
  '行かないだろう。',
  'しない',

  // ぬ (nu) - classical negative (related but different form)
  // Note: These are different from ざる (attributive vs terminal form)
  '行かぬ。',
  '考えぬ。',

  // Note: ざるを得ない SHOULD match this rule since ざる is part of it
  // The full expression ざるを得ない is a separate grammar point that builds on ざる

  // ない (nai) - modern negation (not classical)
  '知らない名画。',
  'たゆまない努力。',

  // ぬ (nu) - classical negative (related but different)
  // These are the terminal/終止形 forms, not 連体形 like ざる
  '行かぬことはできない。',
  '知らぬ間に。',

  // ずに (zuni) - classical negative + te-form
  '行かずに帰った。',
  '食べずに寝る。',

  // ずにはいられない (zuni wa irarenai) - cannot help but (different grammar)
  '泣かずにはいられない。',
  '笑わずにはいられない。',

  // Similar sounding but unrelated words
  'サルがいる。',
  '座る。',  // suwaru (to sit)

  // ないで (naide) - negative te-form
  '食べないでください。',
  '行かないで。',

  // なくて (nakute) - negative te-form
  '食べなくていい。',

  // なければ (nakereba) - negative conditional
  '行かなければ。',

  // Adjectives with -ざる looking ending (rare but possible)
  // '危険ざる',  // Usually 危険な, not *ざる
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
