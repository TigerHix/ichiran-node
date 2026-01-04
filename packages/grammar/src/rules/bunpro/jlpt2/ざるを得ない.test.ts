import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ざるを得ない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ざるを得ない grammar rule
const negatives = [
  // ずにはいられない (zu ni wa irarenai) - "cannot help but" (more subjective/emotional)
  '泣かずにはいられなかった。',
  '笑わずにはいられない。',
  '彼は一言も言わずに部屋を出た。',
  '傘を持たずにでかけた。',

  // ないわけにはいかない (nai wake ni wa ikanai) - "can't not, no way to avoid"
  '行かないわけにはいかない。',
  '勉強しないわけにはいかない。',
  '約束を破るわけにはいかない。',
  '会社を辞めるわけにはいかない。',

  // しかない (shika nai) - "have no choice but, only" (more neutral/conversational)
  'やるしかない。',
  '待つしかない。',
  '諦めるしかない。',

  // てたまらない (te tamaranai) - "can't help but want to, very" (emotional)
  '会いたくてたまらない。',
  '知りたくてたまらない。',
  'うれしくてたまらない。',

  // てしょうがない (te shouganai) - "extremely, really, it can't be helped" (emotional state)
  '寂しくてしょうがない。',
  '疲れてしょうがない。',
  '心配してしょうがない。',

  // てならない (te naranai) - "very, extremely, can't help but" (uncontrollable emotion)
  '会いたくてならない。',
  '心配でたまらない。',
  '不安でならない。',

  // 得ない (enai) - "unable to, cannot" (objective impossibility, different grammar)
  'あり得ない話。',
  '決してあり得ない。',
  'そんなことはありえない。',

  // 〜ざる alone (zaru alone) - classical negative form modifying nouns
  '知られざる名作。',
  '帰らざる者。',
  '変わりざる決意。',

  // Plain negative forms
  '同意しない。',
  '認めない。',
  '行かない。',
  'しない。',

  // Similar sounding but unrelated grammar
  'される。',
  'ざるを見る。',
  'を得る。',

  // ずに (zu ni) - "without doing" (classical form of ないで)
  '何も知らずにあんなこと言ってごめんなさい。',
  '朝ごはんを食べずに仕事に行きました。',
  '水を飲まずに運動をしていたから。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', 'ざるを得ない', engine.get, { negatives });
});
