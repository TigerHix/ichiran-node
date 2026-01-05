import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だけに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the だけに grammar rule
const negatives = [
  // だけ alone (without に) - "only, just"
  // These should NOT match because they don't have the に particle
  'これだけで十分です。',
  '私だけが知っている。',
  'お金だけあれば幸せだ。',
  '読むだけ読んでみて。',
  '朝ごはんはパンだけ食べた。',
  '二人だけで行きました。',
  '水しか飲まない。（only water - using しか～ない）',

  // だけでなく (dakedenaku) - "not only... but also"
  // This is a different grammar pattern
  '彼は日本語だけでなく英語も話せる。',
  'この店は安いだけでなく美味しい。',
  '頭がいいだけでなく性格もいい。',

  // だけあって (dakeatte) - "as might be expected" (positive evaluation only)
  // Similar meaning but different form (used with positive outcomes only)
  'プロの料理だけあって、美味しい。',
  '有名なレストランだけあって、味が素晴らしい。',
  '練習しただけあって、上手だ。',

  // だけは (dakeha) - "only... (but)"
  '価格だけは高い。',
  '見た目だけはいい。',

  // にしては (nishite) - "considering, for"
  '子供にしては詳しい。',
  '新人にしてはよくやっている。',
  'この製品は安いにしては品質が良い。',

  // に (ni) as directional particle - "to, at, in"
  '東京に行きます。',
  '学校にいる。',
  '友達に会う。',
  '日本に住みたい。',

  // に (ni) as indirect object marker
  '彼にプレゼントをあげる。',
  '先生に質問する。',
  '母に電話をかける。',

  // だけに as independent words (different meaning)
  // だけ (only) + に (to/at) in separate contexts
  // Example: それだけにする (Do only that much) - different grammar

  // Similar sounding but unrelated patterns
  // だけの (dakeno) - "only" + possessive
  'できるだけのことをした。',
  '好きなだけの時間を使う。',

  // だけでは (dakedeha) - "only with/by"
  'お金だけでは幸せになれない。',
  '努力だけでは成功できない。',

  // だけでも (dakedemo) - "even just/only"
  '見るだけでも幸せだ。',
  'それだけでも十分です。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
