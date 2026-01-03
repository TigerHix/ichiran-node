import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './う-verb-past.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases: sentences that should NOT match this rule
const negatives = [
  // Ru-verbs (ichidan) in past tense - different conjugation class
  'ご飯を食べた。',      // 食べる (下一段-バ行) - ichidan verb
  '映画を見た。',        // 見る (上一段-マ行) - ichidan verb
  '部屋を出た。',        // 出る (上一段-ラ行) - ichidan verb

  // U-verbs in non-past forms
  '本を読む。',          // dictionary form
  '彼は学校へ行く。',    // dictionary form
  '本を読みます。',      // polite non-past (masu, not mashita)

  // Irregular verbs
  '勉強した。',          // する (サ行変格) - irregular verb
  '来た。',              // くる (カ行変格) - irregular verb
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
