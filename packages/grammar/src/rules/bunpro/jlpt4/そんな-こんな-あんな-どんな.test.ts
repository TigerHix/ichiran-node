import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そんな-こんな-あんな-どんな.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: patterns that look similar but should NOT match
const negatives = [
  // そんなに (with に) - adverbial form, different grammar
  'そんなに面白い。',
  'そんなに高い。',
  // Simple そ/こ/あ/ど + な + noun where な is a different grammatical component
  // (unlikely in practice but showing we're not just matching "な" after a kosoado)
  // その/この/あの/どの + noun - these are simple demonstratives, not "kind of"
  'その本を読む。',
  'この車は速い。',
  'あの人は先生です。',
  'どの色が好きですか。',
  // そういう/こういう/ああいう/どういう - different structure
  'そういうことは言わないで。',
  'どういう意味ですか。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
