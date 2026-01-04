import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことにする.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar patterns that should NOT match
const negatives = [
  // ことになる (JLPT3 - "it is decided that" - external decision, not speaker's volition)
  '来月日本に行くことになった。',
  '会議は来週に行われることになりました。',

  // ことがある (JLPT3 - "sometimes do" - different auxiliary)
  'この馬は人を蹴ることがある。',
  'たまに楽しいことがある。',

  // ことだ (JLPT2 - advice "should")
  '健康のためには運動することだ。',
  '成功するためには努力することだ。',

  // ことにしては ("considering that..." - different grammar)
  '彼は初心者にしては上手だ。',
  'この店は新宿にしては静かだ。',

  // ことになっている ("it is arranged that" - different grammar)
  '授業は9時からことになっている。',
  '靴を脱ぐことになっています。',

  // Simple noun + にする (JLPT5 - "choose/decide on" a noun)
  '私はビールにします。',
  'これにするよ。',

  // ようにする (JLPT4 - "try to/endeavor to" - different nuance)
  '毎朝走るようにしている。',
  '遅刻しないようにしてください。',

  // こと + に without する (incomplete pattern)
  'それは私のことになる。',
  '日本のことについて話す。',

  // たことがある (JLPT5 - past experience)
  '日本に行ったことがある。',
  '寿司を食べたことがあります。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
