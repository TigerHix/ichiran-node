import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './じゃない.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: じゃない should NOT match in these cases
const negatives = [
  // I-adjectives use くない, not じゃない
  '大きくない。',
  '高くない。',
  '新しくない。',

  // Formal ではない should NOT match (different grammar pattern)
  '彼は学生ではない。',
  'あの店は静かではない。',
  'その動物は魚ではないだろう。',
];

// Sentences from Bunpro data that don't actually contain じゃない
// These are polite forms (ではありません, ではない, じゃありません) shown for context
// but our rule is specifically for the casual じゃない form
const skipPositives = [
  'それは飲み物ではありません。',
  'あなたの部屋は綺麗ではありません。',
  '好きなスポーツは、サッカーではありません。',
  'その動物は、魚ではないだろう。',
  'あの店は、静かではない．',
  '今、午前ではありません。',
  'この絵は、綺麗じゃありません。',
  '俺は馬鹿じゃねえよ。',  // Uses じゃねえ (slang pronunciation), not じゃない
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
