import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かと思ったら-かと思うと.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Simple 思う (thinking/pondering) without the conditional pattern
  '彼は来ると思う。',
  '明日は雨だと思います。',
  '彼女は悲しいと思う。',

  // と思っている (currently thinking/state of opinion) - different grammar
  '私は正しいと思っている。',
  '彼は来ると思っている。',

  // と思えば (if one thinks) - different conditional form
  '彼女ならできると思えば、不安も消える。',

  // かと思いきや (contrary to expectation) - different grammar point
  '勝つかと思いきや負けた。',
  'できるかと思いきやできなかった。',

  // としたら (if it were the case that) - different pattern
  '彼が来たとしたら、どうする？',

  // とすると (if we assume that) - different pattern
  'これが本当だとすると、問題だ。',

  // Simple quotation patterns
  '彼は「来る」と言った。',
  '彼女は「大丈夫」と思った。',

  // か + と (question + quotation) without 思う
  '来るかどうか分からない。',
  '行くか行かないか迷っている。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
