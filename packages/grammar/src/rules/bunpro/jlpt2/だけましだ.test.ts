import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけましだ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the だけましだ grammar rule
const negatives = [
  // だけ alone (different grammar - "only/just")
  'これだけあれば十分です。',
  'それだけ知りたい。',
  'お金だけあれば幸せだ。',

  // まし alone (adjective stem "better/preferable" without だけ context)
  // This is rare in isolation but we want to ensure we don't over-match

  // Positive forms of まし (not the "could be worse" construction)
  // まだ (mada) - "still" (different word)
  'まだ仕事が残っている。',
  'まだ雨が降っている。',

  // 増す (masu) - "to increase" (verb form, not adjective stem)
  '人口が増す一方だ。',
  '不安が増す。',

  // だけ + まし in positive context (not "at least it's better")
  // This is a nuanced negative - structurally same but different pragmatics
  // For now, we focus on structural matching

  // Similar constructions with different grammar
  // よりまし (yori mashi) - "better than" ( comparative)
  // This appears in the alternate answers in test data

  // Positive forms ending in だけ but without まし
  'お金があるだけで幸せだ。',
  '行くだけで楽しい。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
