import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ざる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ざる grammar rule
const negatives = [
  // ざるを得ない (zaruoenai) - "have no choice but to, cannot help but"
  // This is a related but different grammar pattern
  'キャンセルせざるを得ない。',
  'やらざるを得ない状況だ。',
  '認めざるを得ない。',

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

  // 独立した「ざる」の使用（補助動詞ではない）
  // Very rare in natural text, but theoretically possible

  // ざる as individual characters (not as compound)
  // Unlikely to occur in natural text
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
