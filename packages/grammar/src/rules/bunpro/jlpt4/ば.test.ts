import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ば.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple verb forms without ば
  '食べる。',
  '行きます。',

  // Simple i-adjective forms without ば
  '高い。',
  'いいです。',

  // Simple noun + だ/です without conditional
  '学生だ。',
  '先生です。',

  // ば as part of words (not conditional)
  '場所はどこですか。',
  '馬に乗る。',

  // Past tense forms (different grammar)
  '食べた。',
  '高かった。',

  // Noun + は (topic particle) - not ば
  '私は学生です。',
  '今日は晴れです。',

  // Verb in te-form + ば doesn't make grammatical sense
  // (not a real Japanese pattern)
  // '食べてば' - this wouldn't occur naturally

  // Dictionary form + ば doesn't occur in Japanese grammar
  // (you need the conjugated form)
  // '食べるば' - this wouldn't occur naturally

  // なら without being conditional (topic marker in some contexts)
  // This is tricky - なら can have multiple uses
  // Let the rule match and rely on context
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
