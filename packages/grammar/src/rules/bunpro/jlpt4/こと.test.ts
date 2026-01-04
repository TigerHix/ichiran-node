import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './こと.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match the nominalizer こと
const negatives = [
  // こと as a regular noun meaning "thing" or "matter" (not following a verb)
  // This is tricky because even as a regular noun it often follows verbs
  // However, we should try to avoid capturing obvious non-nominalizer uses
  'このことは秘密です。',  // "This matter is a secret" - こと is head noun, not nominalizer
  'いいことがありました。',  // "A good thing happened" - ambiguous, but こと is modified by adjective
  '何かことある？',  // "Is there some matter/thing?" - こと as object
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
