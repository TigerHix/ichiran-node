import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しかも.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the しかも grammar rule
const negatives = [
  // しか〜ない ("only/not more than") - different grammar pattern
  '私は百円しか持っていない。',
  'この店でしか買えない。',
  '彼しか知らない。',
  '十分钟しか待てません。',

  // し (shi) - conjunction particle meaning "and/and also" (different grammar)
  '彼は頭もいいし性格もいい。',
  'この店は安いし美味しい。',
  '雨も降るし風も吹く。',

  // か (ka) - question particle
  'あなたは行きますか。',
  '何時ですか。',
  '分かりますか。',

  // も (mo) - "also/too" particle
  '私も行きます。',
  'これも好きです。',
  '彼も来るでしょう。',

  // Similar conjunctions with different meanings
  // その上 (sono ue) - "moreover" (more formal, neutral)
  '彼は優秀だ。その上、人柄もいい。',
  '顔が青白い。その上、唇は紫だ。',

  // それに (soreni) - "and besides" (neutral, less formal)
  '彼は優秀だ。それに性格もいい。',
  'この店は安い。それに美味しい。',

  // おまけに (omake ni) - "on top of that" (often negative, colloquial)
  '雨だった。おまけに風も強かった。',
  '遅刻した。おまけに宿題も忘れた。',

  // 更に (sara ni) - "further still" (progression/escalation)
  'さらに悪い結果になった。',
  'さらに詳しく説明します。',

  // なお (nao) - "furthermore" (neutral, formal, simple addition)
  '詳細はなおお問い合わせください。',
  'なお、明日も営業しております。',

  // そして (soshite) - "and then/also"
  '彼は日本語が上手。そして、漢字も綺麗に書ける。',
  '本を読んだ。そして、寝た。',

  // また (mata) - "also/again"
  '彼はシンガーだ。また、俳優でもある。',
  '明日また来てください。',

  // それとも (soretomo) - "or" (alternative question)
  'コーヒーにしますか。それとも、紅茶にしますか。',
  '行くか。それとも、行かないか。',

  // しかし (shikashi) - "however/but"
  '彼は貧しかった。しかし、幸せだった。',
  '頑張った。しかし、失敗した。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
