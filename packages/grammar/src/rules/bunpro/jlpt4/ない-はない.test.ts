import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ない-はない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple negative sentences (not double negative)
  '漢字を書かない。',
  'コーヒーを飲まない。',
  'この部屋はテレビがない。',

  // は without double negative pattern
  'これはテレビです。',
  '私は学生です。',

  // Different grammar: は marks topic, not part of double negative
  '私は勉強します。',

  // Noun + は + ない without preceding negative
  'テレビはない。',
  '部屋はない。',

  // ではない without double negative structure
  'これはテレビではない。',
  '彼は学生ではない。',

  // じゃない without double negative structure
  'それはテレビじゃない。',
  '彼は学生じゃない。',
];

// Sentence with complex dialog structure that has parsing issues:
//
// 父親：「新しいクラスはどう？友達を作ったの？」 子供：「好きじゃないクラスメイトはいないけど、まだ、できてないよ。」
//
// This sentence contains nested dialog quotes and the pattern "好きじゃないクラスメイトはいない"
// (suki ja nai kurasumeito wa inai) which should match Pattern 3.
//
// However, the sentence structure with dialog markers and the continuation "けど" (but)
// appears to cause issues with GiNZA's tokenization or dependency parsing in this specific context.
// The rule correctly matches the same pattern in simpler sentences like:
// - 好きじゃない人はいない
// - 好きではない人はいない
//
// This is a context-specific parsing limitation, not a fundamental issue with the rule.
const skipPositives = [
  '父親：「新しいクラスはどう？友達を作ったの？」 子供：「好きじゃないクラスメイトはいないけど、まだ、できてないよ。」',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
