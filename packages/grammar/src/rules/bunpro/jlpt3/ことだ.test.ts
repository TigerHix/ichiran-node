import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことだ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests: similar patterns that should NOT match
const negatives = [
  // たことがある (JLPT5 - past experience "have done before")
  // This uses verb in past tense (た-form) + ことがある
  '日本に行ったことがある。',
  'このラーメンは食べたことがあると思う。',

  // ことがある (JLPT3 - "sometimes do" or "there are times when")
  // This uses verb in dictionary form + ことがある (with ある, not だ)
  'この馬は人を蹴ることがある。',
  'たまに楽しいことがある。',

  // ことはない (JLPT3 - "there is no need to" or "never happens")
  '彼と話すことはない。',
  '心配することはない。',

  // ことになる (JLPT3 - "it is decided that")
  '来月日本に行くことになった。',

  // ことにする (JLPT3 - "decide to")
  '毎日運動することにしました。',

  // ものだ (JLPT3 - "supposed to" or "that's how it is")
  '子供は泣くものだ。',
  '水は高いところから低いところへ流れるものだ。',

  // Simple こと as a noun (different usage)
  'これは私のことです。',
  '大切なことを忘れていた。',

  // ことができる (can do - potential form)
  '私は日本語を話すことができます。',
  '彼はピアノを弾えることがある。',

  // ことになる (different meaning - "it turns out that")
  '彼来ないことになる。',

  // ことになっている (rule/arrangement)
  'この部屋では喫煙は禁止されていることになっている。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
