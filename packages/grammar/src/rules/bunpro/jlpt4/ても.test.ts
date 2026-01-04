import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ても.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match ても
const negatives = [
  // Simple も as "also/too" (not even if)
  '私も行きます。',
  'これも好きです。',
  '彼も学生です。',

  // て-form without も (different grammar)
  '本を読んでいます。',
  '朝ごはんを食べて学校に行きます。',

  // でも as "but" (conjunction, not even if)
  'すみません、でも無理です。',
  '雨が降っている。でも、行きます。',

  // でも as "or something" (particle after question words)
  // Note: These are covered by the separate "でも" grammar point (JLPT4)
  // '何でも食べます。',
  // 'どこでもいいです。',

  // てしまう (not ても)
  '食べてしまった。',
  '忘れてしまった。',

  // てくる (not ても)
  '雨が降ってきた。',
  '彼が走ってきた。',

  // ている (not ても)
  '本を読んでいる。',
  '待っている。',

  // ておく (not ても)
  '予約しておく。',
  '覚えておく。',

  // てあげる (not ても)
  '貸してあげる。',
  '教えてあげました。',

  // てもいい (permission, different grammar)
  // Note: This is covered by the separate "Verb + てもいい" grammar point (JLPT5)
  // There may be some overlap, but the てもいい rule should take precedence
  // '行ってもいいです。',
  // '食べてもいいですよ。',

  // なければならない (obligation, not even if)
  '勉強しなければならない。',
  '行かなければなりません。',

  // てはいけない (prohibition, not even if)
  'ここに入ってはいけない。',
  '写真を撮ってはいけません。',

  // ば conditional (not even if)
  '行けばわかる。',
  '勉強すれば合格できる。',

  // たら conditional (not even if)
  '行ったら会える。',
  '雨が降ったら中止です。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
