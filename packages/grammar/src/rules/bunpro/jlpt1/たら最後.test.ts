import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たら最後.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // 最後 as a standalone noun (not connected to verb + tara/ga pattern)
  'これは最後だ。',
  '最後まで諦めないでください。',
  '最後に一言言わせてください。',

  // 〜てから patterns (different grammar: "after doing X")
  '日本に来てから、十年になる。',
  '帰ってから、電話します。',

  // た form followed by other particles (not が or ら + 最後)
  '言った時、彼は笑った。',
  '食べた後、すぐ寝た。',
  '行ったら、連絡してください。', // Different meaning: "if you go"

  // Simple 最後 compound (not this grammar point)
  '最後の晩餐',
  '最後のチャンス',

  // たら as conditional without 最後
  '雨が降ったら行かない。',
  'お金があったら買います。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
