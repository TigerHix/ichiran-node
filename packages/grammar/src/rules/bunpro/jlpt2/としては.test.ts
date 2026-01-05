import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './としては.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the としては grammar rule
const negatives = [
  // Simple topic は (without として)
  '東京は大きな都市です。',
  '日本は島国です。',
  '彼は学生です。',
  'これは本です。',

  // Locative では (de + wa, not toshite + wa)
  '東京では電車が便利です。',
  '日本では桜が有名です。',
  'ここではタバコを吸わないでください。',
  '私の国では、そういうことは珍しい。',

  // ではない (negation, different grammar)
  '彼は学生ではない。',
  'これは間違いではない。',
  'あそこは学校ではない。',

  // にしては (nishite - "considering, for" with different nuance)
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',
  'この製品は安いにしては品質が良い。',
  '彼にしては静かだ。',

  // には (niwa - "for, in regard to" emphasizing fundamental relation)
  '私には分からない。',
  'あなたには難しいかもしれない。',
  'それは私には関係ない。',

  // にとって (nitotte - "to, for" emphasizing relevance/effect)
  '学生にとって分かりにくい説明だ。',
  '私にとって大切な人です。',
  '子供にとって安全な場所だ。',

  // としても (toshitemo - "even as" conditional)
  '先生としても反対だ。',
  '友達としても許せない。',
  '冗談としても言い過ぎだ。',

  // として alone (without は, JLPT3 grammar)
  '先生としての彼。',
  '友達として付き合う。',
  '会社としての目標。',

  // と + して + は in different grammatical contexts
  // Quote + do + topic (not "as")
  '「する」と言った人は彼です。',
  '彼は来ると言っていたはずだ。',

  // Instrumental で + topic は (different particle)
  '車では行けない場所です。',
  '日本語では翻訳できない。',
  'この道具では開かない。',

  // て-form of other verbs + は (not して + は)
  '食べてはいけない。',
  '行ってはならない。',
  '使ってはいけないものだ。',

  // Verb + としては (grammatically different structure)
  // These would be verb + quoting particle + te-form + topic
  '言うとしては簡単だ。',
  'やるとしては難しい。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
