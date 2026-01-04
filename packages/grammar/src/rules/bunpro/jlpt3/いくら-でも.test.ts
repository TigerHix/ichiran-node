import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いくら-でも.js';
import { BUNPRO_JLPT3 } from './index.js';

// Sentences with いくら that should NOT match the "no matter how" pattern
const negatives = [
  // いくら as question word "how much" (not "no matter how much")
  'これはいくらですか。',
  'おいくらですか。',
  'いくらありますか。',
  // いくら as amount (not "no matter how")
  'いくら使いましたか。',
  'いくら欲しいですか。',
  // いくら + particle (not でも/ても)
  'いくらから買えます。',
  'いくらと交換します。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
