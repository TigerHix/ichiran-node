import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かどうか.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: similar-looking patterns that should NOT match
const negatives = [
  // Simple question with just か (not the embedded question かどうか pattern)
  '行きますか。',
  '食べるか。',
  'これは本ですか。',
  // どうか without the first か
  'どうかよろしくお願いします。',
  'どうか助けてください。',
  // Separate instances of か and どう
  'これは何か。どうすればいい？',
  // か as a particle marking a quote or list item
  'リンゴかバナナかを選ぶ。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
