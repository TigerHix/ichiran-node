import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ということ.js';
import { BUNPRO_JLPT4 } from './index.js';

// Sentences that should NOT match ということ (nominalizer)
const negatives = [
  // という + noun (NOT followed by こと) - "called X" pattern
  // This is the という-called grammar point (JLPT3)
  'ポケモンというゲーム',  // a game called Pokemon
  '佐藤という人',  // a person named Sato

  // Simple こと (JLPT4) - nominalizer without という
  '日本語を話すことは難しい',  // Speaking Japanese is difficult (simple nominalizer, no という)

  // という followed by other particles (not こと)
  '彼が来るという報告',  // The report that he is coming (という + 報告, not こと)

  // Note: "失敗するということはある" (There are times when one fails)
  // This DOES contain ということ as a valid nominalizer.
  // Even though the full pattern is related to というのは (JLPT3),
  // the substring ということ itself is a valid match for our rule.
  // So it's intentionally NOT included in negatives.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
