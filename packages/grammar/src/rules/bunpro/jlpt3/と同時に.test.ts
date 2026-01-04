import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と同時に.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple と (quotative/comparative) without 同時に
  '彼と言った。',
  '私は行きたいと言った。',

  // と (with) marking separate items without simultaneity nuance
  '本とペンを買った。',
  '友達と映画を見た。',

  // 同時 alone (different grammar)
  '同時に始めましょう。',

  // に alone (different grammar)
  'ここに行きます。',

  // Similar patterns that shouldn't match
  '彼と同時に来た。',  // Simple "with" (quotative or similar), not "at the same time"
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
