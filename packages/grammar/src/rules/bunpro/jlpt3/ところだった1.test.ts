import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ところだった1.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // たところだ (JLPT4) - "just finished doing" (different tense/mood)
  // This is a DIFFERENT grammar point - uses verb ta-form
  '今帰ったところです。',
  '食べたところでケーキを出された。',

  // るところだ (JLPT4) - "about to do now" (present tense, not past)
  // Uses present copula だ/です, not past だった
  '今出発するところだ。',

  // ところ as a regular noun meaning "place"
  'いいところを見つけた。',
  'ここは静かなところです。',

  // にする (to decide on/choose) - completely different grammar
  'これにする。',

  // だった (past copula) without ところ
  '学生だった。',
  '彼は先生だった。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
