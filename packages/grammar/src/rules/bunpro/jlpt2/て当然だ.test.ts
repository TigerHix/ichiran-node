import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './て当然だ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the て当然だ grammar rule
const negatives = [
  // 当然 alone (without te-form context)
  'それは当然の結果だ。',
  '成功するのは当然だ。',
  '努力すれば当然の報いがある。',

  // Verb/adj-te form without 当然 (te-form for different reasons)
  'ご飯を食べて寝た。',
  '公園を歩いて学校に行った。',
  'この部屋は広くて明るい。',

  // Similar grammar: のも当然だ (nominalized form - different rule)
  // These are structurally different but semantically similar
  // The noun marker の makes it a different grammatical construction

  // Similar grammar: のももっともだ (empathetic understanding)
  '怒るのももっともだ。',
  '不安になるのももっともだ。',

  // 当然 used as adverb (naturally/of course) without copula
  '彼は当然知っているはずだ。',
  '当然のことながら、',

  // で without 当然 (instrumental/locative で, not copula te-form)
  '電車で行く。',
  '日本語で話す。',
  '鉛筆で書く。',

  // Similar expressions with different vocabulary
  // てあたりまえ (te ataramae) - "it's a matter of course" (synonym)
  // We don't test these as they're semantically equivalent but different surface forms

  // Positive contexts with different meanings
  '当然の権利だ。',
  '当然のように振る舞う。',

  // Verb/adj + だ without 当然
  '彼は来た。',
  'この部屋は広い。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
