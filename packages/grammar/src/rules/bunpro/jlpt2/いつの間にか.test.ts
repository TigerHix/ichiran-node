import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いつの間にか.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar expressions that are NOT いつの間にか
  // あっという間に (similar meaning but different grammar)
  '楽しいことをしているとあっという間に時間が過ぎるよね。',
  // いつ + に without the full phrase
  'いつに行くか決めてください。',
  // そのうちに (different grammar - "その" instead of "いつ")
  'そのうちに雨が止むでしょう。',
  // 間に (time phrase without いつの)
  '授業の間に宿題を終わらせた。',
  // Negative: か used as question particle, not part of いつの間にか
  'いつの間に行けるかわかりません。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
