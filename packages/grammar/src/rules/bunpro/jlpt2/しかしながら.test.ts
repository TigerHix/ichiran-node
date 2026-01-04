import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しかしながら.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the しかしながら grammar rule
const negatives = [
  // しかし (shikashi) - "however" without ながら (less formal)
  '努力した。しかし、失敗した。',
  '確かに便利だ。しかし、高価だ。',

  // ですが (desuga) - "but" (polite, conversational)
  'それは良いアイデアです。でも、高価です。',
  '行きたいです。ですが、時間がありません。',

  // だが (daga) - "but" (plain, conversational)
  '頑張った。だが、失敗した。',
  '雨が降った。だが、試合は続いた。',

  // ところが (tokoroga) - "however" (less formal)
  '失敗した。ところが、また挑戦した。',
  '店は閉まっていた。ところが、隣の店は開いていた。',

  // それなのに (sorennoni) - "and yet" (expresses surprise/disappointment)
  '勉強した。それなのに、テストが悪かった。',
  '約束した。それなのに、来なかった。',

  // もっとも (mottomo) - "however" (adds qualification)
  '全員来ました。もっとも、田中さんは遅れましたが。',
  '安いです。もっとも、品質は良くありません。',

  // だけど (dakedo) - "but" (conversational)
  '疲れた。だけど、楽しかった。',
  '雨だ。だけど、行く。',

  // ですけど (desukedo) - "but" (conversational, polite)
  '分かりません。ですけど、頑張ります。',
  '高いです。ですけど、買います。',

  // しかし + other particles (not ながら)
  'しかしの話は聞きたくない。',
  'しかしについて説明します。',

  // ながら (nagara) alone or with other words
  '音楽を聞きながら勉強する。',
  '歩きながら食べる。',

  // 併し (rare kanji form of しかし) without 乍ら
  '併し、それは間違いだ。',

  // Words containing 部分 but not meaning "however"
  // No obvious false positives here
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
