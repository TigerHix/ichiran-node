import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './へいく.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // にいく (go to do something) - different grammar pattern
  '食べに行く。',
  '映画を見に行きます。',
  '勉強しに来た。',
  // へ alone (directional particle without motion verb)
  '学校へは行きたくない。',
  '東京へ行ったことがあります。',
  // に行く (destination-focused, not journey-focused)
  '学校に行く。',
  '病院に行きます。',
  // Other motion verbs without へ
  '学校に行く。',
  '家に帰る。',
  '駅まで歩く。',
];

// Positive sentences to skip (not actually examples of へいく pattern)
//
// The Bunpro data includes contrastive examples showing へ vs に:
// - "エルサは病院にいく" demonstrates the に particle (destination focus)
// - "ジョンは学校へいく" demonstrates the へ particle (journey focus)
//
// These are included in the same JSON file for pedagogical reasons,
// but only sentences with へ in the answer should test the へいく rule.
const skipPositives = [
  'エルサは病院にいく。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
