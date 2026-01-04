import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './のなかで.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: should NOT match
const negatives = [
  // 中 without で (just "middle" as a noun, not the grammar pattern)
  '箱の中に猫がいる。',
  '部屋の中に入る。',
  // 中に (directional "into", not scope "among")
  '部屋の中に行く。',
  '池の中に入る。',
  // で as instrumental or locative without 中, not "among/within"
  '家で勉強する。',
  '鉛筆で書く。',
  '車で行く。',
  // 中 as "during" with time-based activities (different reading: ちゅう)
  '授業中は静かにしてください。',
  '仕事中は電話ができない。',
  // 中 as suffix meaning "during" (chū reading, different from なか)
  '会議中に電話があった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
