import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てもかまわない.js';
import { BUNPRO_JLPT3 } from './index.js';

// Sentences that should NOT match the pattern
const negatives = [
  // ても without かまわない (different grammar)
  '雨が降っても行きます。',
  '忙しくても時間を作ります。',
  // でも as "but" conjunction (not permission pattern)
  '行きたいですが、時間がありません。',
  '彼は来ましたが、彼女は来ませんでした。',
  // かまわない without ても (plain negative)
  '彼は全然かまわないと言いました。',
  // てもいい (permission, not indifference)
  'ここに座ってもいいです。',
  '入ってもいいですか。',
  // Noun + でも as emphatic particle (not permission)
  '子供でもできる。',
  'プロでも失敗することはある。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
