import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ていく.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative tests: てくる (opposite direction - should NOT match)
const negatives = [
  // てくる patterns (movement toward speaker)
  '持ってくる',
  '食べてくる',
  '買ってくる',
  '連れてくる',
  'もってきてください',
  '持ってきた',
  '食べてきた',
  // Standalone いく (not following verb-te)
  '京都へいく',
  '学校にいく',
  'どこへいくんですか',
  // いきました/いった as simple past of いく (not verb-te+iku)
  '京都へいきました',
  '彼はもういった',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
