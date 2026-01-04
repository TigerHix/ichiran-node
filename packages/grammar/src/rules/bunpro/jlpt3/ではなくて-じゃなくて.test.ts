import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ではなくて-じゃなくて.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences with similar-looking patterns that should NOT match
const negatives = [
  // ではない (copula negation, NOT conjunctive form)
  // This is the plain negation "is not", not "not X but Y"
  '彼は学生ではない。',
  'それは間違いではない。',

  // Locative では (in/at location, NOT conjunction)
  // Uses では as a topic marker in a location context
  '東京では雨が降っています。',
  '図書館では静かにしてください。',
  'この店ではコーヒーを売っている。',

  // Instrumental で (by means of, NOT conjunction)
  '車で行きます。',
  '鉛筆で書きました。',
  '日本語で話してください。',

  // Conjunctive なくて (verb/い-adj negative te-form, NOT noun negation)
  // This is なくて from verbs/い-adjectives, not ではなくて
  '行けなくて残念です。',
  '安くなくて買わなかった。',
  '勉強しなくて困った。',

  // じゃない (plain negation, NOT conjunctive)
  'それは夢じゃない。',
  '今日は休みじゃない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
