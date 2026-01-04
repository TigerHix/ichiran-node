import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずに済む.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ずに済む grammar rule
const negatives = [
  // ずに alone (without 済む) - "without doing"
  // This is the related JLPT3 grammar without the resolution aspect
  '朝ごはんを食べずに仕事に行きました。',
  '何も知らずにあんなこと言ってごめんなさい。',
  '値段を見ずに買ったら大変なことになった。',
  '水を飲まずに運動をしていたから。',
  '勉強せずにテストを受けた。',

  // ないで (naide) - modern "without doing" (different grammar)
  '彼は知らないで言った。',
  '私は食べないで行った。',
  '傘を持たないで出かけた。',
  '歯を磨かないで寝た。',

  // なくて (nakute) - te-form of negative (different grammar)
  '勉強しなくてはいけない。',
  'お金がなくて買えない。',
  '時間がなくて行けなかった。',

  // ずに followed by verbs other than 済む
  '休まず、一日中ゲームをやり続けた。',
  '試合に一回も負けず、優勝した。',
  '力まずにスウィングした。',
  '諦めずに続ければ。',
  '気を緩めず、次の試合も頑張って。',

  // 済む (sumu) used independently - "to finish, to end"
  '宿題が済んだ。',
  '手続きが済みました。',
  '試合が済んでから帰ります。',
  '準備が済んでいる。',

  // なしで (nashide) - different pattern (can follow nouns)
  // Note: This is a valid alternate for ずに済む but different construction
  '今年の冬は去年より暖かかったので、ヒーターなしで済みました。',
  // This should match a different rule for なしで済む

  // Similar patterns with different meanings
  // にしては (nishite) - "considering, for"
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',

  // にとって (nitotte) - "for, in the case of"
  '私にとって大切な人。',
  '子供にとって難しい問題。',

  // ず (zu) as numeral counter
  '一ずつ',
  '二ずつ',

  // する becomes せ (se) - different pattern
  '成功するために努力せよ。',
  'よく考えせばわかる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
