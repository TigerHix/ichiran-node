import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './zaru.ts';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ざる grammar rule
const negatives = [
  // Modern ない forms (not archaic ざる)
  '知らない人。',
  '得ない結果。',
  '消えない傷。',
  '絶えない失敗。',
  'たゆまない努力。',

  // ずに (zuni) - "without doing" (adverbial use, not attributive)
  '何も知らずに言った。',
  '朝ごはんを食べずに行った。',
  '水を飲ずに運動した。',

  // ず (zu) - classical negative in other forms
  '知らず知らずのうちに。',
  '休まず働いた。',
  '負けずに頑張る。',

  // Similar sounding but unrelated words
  // さる (saru) - monkey
  '猿が木の上にいる。',
  '山には猿が多い。',

  // Note: ざるを得ない patterns are NOT in negatives because:
  // - ざる is technically a valid instance of the classical negative form
  // - The ざるを得ない rule will also match these sentences
  // - Applications can choose which match to use based on their needs
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
