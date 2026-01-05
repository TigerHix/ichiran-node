import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずに済む.js';
import { BUNPRO_JLPT2 } from './index.js';

// Sentences that can't be matched due to alternate grammar patterns:
//
// The Bunpro data includes alternate forms with ないで and なしで:
// - ずに済む (main pattern - classical form)
// - ないで済む (modern equivalent)
// - なしで済む (can follow nouns)
//
// However, implementing patterns for ないで and なしで causes overcapture:
// - ないで is too general - matches many unrelated "without doing" constructions
// - なしで following nouns requires different matching logic
//
// The main pattern (ずに済む) is the classical/literary form and is the primary
// grammar point being tested here.
const skipPositives = [
  '今月契約すると初期費用を支払わないですむので、今月中に契約することをお勧めします。',
  '今年の冬は去年より暖かかったので、ヒーターなしですみました。',
];

const negatives = [
  '朝ごはんを食べずに仕事に行きました。',
  '何も知らずにあんなこと言ってごめんなさい。',
  '値段を見ずに買ったら大変なことになった。',
  '水を飲まずに運動をしていたから。',
  '勉強せずにテストを受けた。',
  '彼は知らないで言った。',
  '私は食べないで行った。',
  '傘を持たないで出かけた。',
  '歯を磨かないで寝た。',
  '勉強しなくてはいけない。',
  'お金がなくて買えない。',
  '時間がなくて行けなかった。',
  '休まず、一日中ゲームをやり続けた。',
  '試合に一回も負けず、優勝した。',
  '力まずにスウィングした。',
  '諦めずに続ければ。',
  '気を緩めず、次の試合も頑張って。',
  '宿題が済んだ。',
  '手続きが済みました。',
  '試合が済んでから帰ります。',
  '準備が済んでいる。',
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',
  '私にとって大切な人。',
  '子供にとって難しい問題。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives, skipPositives });
});
