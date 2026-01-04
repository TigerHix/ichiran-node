import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './では-それでは-じゃあ.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with では/じゃ that should NOT match the conjunction pattern
const negatives = [
  // Locative では (で=case marker for location, not conjunction)
  '東京では電車が便利です。',
  '日本では桜が有名です。',
  'この店では現金しか使えない。',
  // ではない negation pattern (different grammar)
  '彼は学生ではない。',
  'これは本ではありません。',
  // Conditional じゃ (ないじゃないか etc)
  '行かないじゃないか。',
  // Instrumental で + topic は
  '車では行けない場所です。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
