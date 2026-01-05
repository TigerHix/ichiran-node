import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と考えられる.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the と考えられる grammar rule
const negatives = [
  // Simple と quoting with different verbs (say, think, etc.)
  '彼が来ると思っています。',
  '先生がそう言った。',
  '成功すると確信しています。',
  '彼はお金を持っていると言った。',
  '私はそれが正しいと思う。',

  // Subjective opinion: と思われる (to omowareru) - more subjective
  '私には彼がこの事件の犯人だと思われる。',
  '私にはあなたが悪いと思われる。',
  'その結論は間違っていると思われる。',

  // と as "with" or "and" (companion particle)
  '友達と映画を見に行った。',
  '彼と話し合った。',
  '家族と食事をします。',

  // Similar patterns but different grammar
  'この件について考えるべきだ。',
  '彼はよく考えている。',
  'もう少し考えさせてください。',

  // Continuous form: と考えられている (widely held opinion)
  // NOTE: Our rule uses `not` constraints to exclude this pattern
  'この方法は最善だと考えられている。',
  '地球は丸いと考えられている。',
  'その薬は効果があると考えられている。',

  // Potential form without と: 考えられる (can think/can imagine)
  'そんなことは考えられない。',
  'この問題は解決策が考えられない。',
  '彼が成功するとは考えられなかった。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
