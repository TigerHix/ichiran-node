import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずに済む.js';
import { BUNPRO_JLPT2 } from './index.js';

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
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
