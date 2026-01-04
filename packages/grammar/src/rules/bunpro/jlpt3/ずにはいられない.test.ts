import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずにはいられない.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative test cases - sentences that should NOT match the ずにはいられない grammar rule
// These test similar patterns that have different meanings
const negatives = [
  // てはいられない (te wa irarenai) - "can't afford to" or "unable to"
  // Different grammar pattern, similar structure but different meaning
  '待ってはいられない。',
  'こんなに遅くまで寝てはいられない。',
  '黙ってはいられない。',

  // ずに (zu ni) - "without doing" (different from ずにはいられない)
  '彼は一言も言わずに部屋を出た。',
  '傘を持たずにでかけた。',
  '朝ごはんを食べずに学校に行きました。',

  // ざるを得ない (zaru o enai) - "have no choice but to" (more objective tone)
  '同意せざるを得ない。',
  '謝罪せざるを得ない。',

  // ないわけにはいかない (nai wake ni wa ikanai) - "must not" or "can't not"
  '行かないわけにはいかない。',
  '勉強しないわけにはいかない。',

  // Simple negative form with ず but different structure
  'さようず。',
  '思いもよらず。',

  // Different auxiliary patterns
  '言わずもがな。',
  '知らず知らずのうちに。',

  // てしょうがない (te shouganai) - "can't help but feel" (emotional state)
  '暑くてしょうがない。',
  '会いたくてしょうがない。',

  // てたまらない (te tamaranai) - "can't help but want to" (overwhelming feeling)
  '暑くてたまらない。',
  '会いたくてたまらない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
