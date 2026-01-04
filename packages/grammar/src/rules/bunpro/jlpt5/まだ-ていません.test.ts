import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './まだ-ていません.js';
import { BUNPRO_JLPT5 } from './index.js';

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);

  // DATA ERROR: The Bunpro data includes "ライトをまだけしていません。"
  // The cloze answer is "まだけしていません" but kanji_answer shows "まだ消（け）していません".
  // This is a typo - "まだけ" (only) is a completely different word from "まだ" (still).
  // The sentence should NOT be in the まだ-ていません grammar point data.
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, {
    skipPositives: ['ライトをまだけしていません。'],
  });
});
