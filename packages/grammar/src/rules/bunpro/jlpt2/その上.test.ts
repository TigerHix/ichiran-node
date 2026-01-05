import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './その上.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the その上 grammar rule
const negatives = [
  // その (sono) - demonstrative alone (not the conjunction)
  'その本はとても面白い。',
  'その店は明日開きます。',

  // 上 (ue) - "top, above" as a noun or direction
  '机の上に本がある。',
  '上を見て歩きなさい。',
  '山の上に雪があります。',

  // Similar conjunctions with different meanings
  // それに (soreni) - "and besides" (neutral, less formal)
  '彼は優秀だ。それに性格もいい。',

  // しかも (shikamo) - "moreover" (less formal, emphatic)
  'この店は安い。しかも美味しい。',

  // おまけに (omake ni) - "on top of that" (often negative, colloquial)
  '雨だった。おまけに風も強かった。',

  // 更に (sara ni) - "further still" (progression/escalation)
  'さらに悪い結果になった。',
  '電気代がさらに高くなった。',

  // なお (nao) - "furthermore" (neutral, formal, simple addition)
  '詳細はなおお問い合わせください。',

  // 上に (ue ni) - "in addition to" (prepositional, within sentence)
  'このパソコンは安い上に性能もいい。',

  // Noun + 上 meaning "on top of/above" (literal spatial meaning)
  'テーブルの上に猫がいる。',
  '地図の上の場所を探す。',

  // Context where その上 appears but as "that top" (not conjunction)
  'その上の段を取ってください。',
  'その上にある部屋は私の部屋です。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
