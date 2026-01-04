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
];

// Sentences that should match but are skipped due to GiNZA limitations
const skipPositives = [
  '俺は馬鹿じゃねえよ。',  // Uses じゃねえ (slang pronunciation), not じゃない
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
