import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './というより.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar patterns that should NOT match
const negatives = [
  // より (JLPT4) - simple comparison "more than" without という
  // 月曜日より水曜日がいい (Wednesday is better than Monday)
  '月曜日より水曜日がいい。',
  '走るより野球をするのが好き。',

  // という (quotation/called pattern) - "called" or "named"
  // 田中という人 (a person called Tanaka)
  '田中という人から電話がありました。',
  'これは何という花ですか。',

  // といっても (even if you say / although it's called)
  // Different grammar point with conditional meaning
  '安いといっても、まだ高い。',

  // というのは (topic marker - "what's called")
  '寿司というのは、日本の代表的な料理です。',

  // とかいう (things like / or something)
  '田中とかいう人に会った。',

  // 単なる より (just "more than" without "to iu")
  '彼は私より背が高い。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
