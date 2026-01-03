import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './の.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that should NOT match the possessive/nominalizer の.
const negatives = [
  // Other particles (が, を, に, etc.) - different case markers
  '私が学生です。',
  '本を読みます。',
  '学校に行きます。',
  '東京で勉強します。',
  '友達と話します。',
  // Topic marker は (different grammatical function)
  '私は学生です。',
  'これはペンです。',
  // Emphatic particles (よ, ね, etc.)
  'そうですよ。',
  'いいですね。',
  // Conjunctive て (te-form connector)
  '食べて寝ます。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
