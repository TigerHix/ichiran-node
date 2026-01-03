import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-non-past.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases: verb forms that should NOT match the non-past pattern
const negatives = [
  // Polite forms (〜ます) - should NOT match non-past
  '見ます。', // mimasu - polite form of 見る
  '食べます。', // tabemasu - polite form of 食べる
  '行きます。', // ikimasu - polite form of 行く
  '勉強します。', // benkyoushimasu - polite form of 勉強する
  '来ます。', // kimasu - polite form of 来る

  // Past tense (〜た/〜だ) - should NOT match non-past
  '食べた。', // tabeta - past form of 食べる
  '行った。', // itta - past form of 行く
  'した。', // shita - past form of する
  '来た。', // kita - past form of 来る

  // Polite past tense (〜ました) - should NOT match non-past
  '食べました。', // tabemashita - polite past form of 食べる
  '行きました。', // ikimashita - polite past form of 行く
  'しました。', // shimashita - polite past form of する
  '来ました。', // kimashita - polite past form of 来る

  // Negative forms (〜ない) - should NOT match non-past
  '食べない。', // tabenai - negative form of 食べる
  '行かない。', // ikanai - negative form of 行く
  'しない。', // shinai - negative form of する
  'こない。', // konai - negative form of 来る

  // Polite negative (〜ません) - should NOT match non-past
  '食べません。', // tabemasen - polite negative form of 食べる
  '行きません。', // ikimasen - polite negative form of 行く
  'しません。', // shimasen - polite negative form of する
  'きません。', // kimasen - polite negative form of 来る

  // Te-form (〜て) - should NOT match non-past
  '食べて。', // tabete - te-form of 食べる
  '行って。', // itte - te-form of 行く
  'して。', // shite - te-form of する
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
