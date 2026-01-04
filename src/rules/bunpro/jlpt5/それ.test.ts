import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それ.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: Other demonstratives that should NOT match
const negatives = [
  // これ (this - near speaker)
  'これはペンです。',
  'これもいいです。',
  // あれ (that over there - far from both)
  'あれは何ですか。',
  'あれも古い。',
  // この (this - before noun)
  'この本は面白い。',
  // その (that - before noun)
  'その本は面白い。',
  // あの (that over there - before noun)
  'あの本は面白い。',
  // ここ (here - place)
  'ここは静かです。',
  // そこ (there - place)
  'そこは静かです。',
  // あそこ (over there - place)
  'あそこは静かです。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
