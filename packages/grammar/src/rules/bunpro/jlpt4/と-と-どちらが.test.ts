import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と-と-どちらが.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple と as quotative particle (not comparison)
  '彼は行くと言った。',
  '明日は雨だと思う。',
  // Simple と as 'with' (accompaniment)
  '友達と行く。',
  '家族と話す。',
  // どちらが without comparison context (polite "who/which")
  'すみません、どちら様ですか？',
  // Single と (not a comparison between two things)
  'りんごが好きです。',
  // と as 'and' in a list (not followed by どちらが)
  '本とペンとノートを買った。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
