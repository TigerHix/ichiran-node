import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でございます.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Regular です (polite copula) - not very polite
  'こちらは図書館です。',
  'この方は田中さんです。',
  '本日のスペシャルはホットケーキです。',
  'お冷やです。',
  'この商品は本当に便利です。',

  // Regular だ (casual copula) - not polite
  'これはペンだ。',
  '彼は学生だ。',
  '今日は休みだ。',

  // ございます (polite form of ある, not copula)
  '時間がございます。',
  'トイレはこちらにございます。',
  'ありがとうございます。',
  'おはようございます。',

  // Locative で (at/in with) - not copula
  '東京で働く。',
  '鉛筆で書く。',
  '電車で行く。',

  // Te-form で + あります (state) - different grammar
  'ドアが閉めてあります。',
  '準備してあります。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
