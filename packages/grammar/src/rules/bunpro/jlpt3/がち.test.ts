import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がち.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // が (subject marker) + unrelated words (not がち)
  '彼が持っている。', // が (subject) + 持っている (has)
  '私が遅刻した。', // が (subject) + 遅刻した (was late)

  // Similar patterns without がち suffix
  '彼が父です。', // が (particle) only
  '家が高い。', // が (particle) + adjective

  // Note: "勝つがち" (tends to win) IS a valid use of がち suffix
  // The kanji 勝ち for がち reflects the etymology, but it's still the suffix
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
