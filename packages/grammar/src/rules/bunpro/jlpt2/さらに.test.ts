import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さらに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the さらに grammar rule
const negatives = [
  // さら (sara) - different word (noise, state of being pure, etc.)
  '騒音がさらさら聞こえる。',

  // に (ni) - particle/direction marker alone
  '東京に行きます。',
  '彼に本をあげる。',

  // Similar conjunctions with different meanings
  // その上 (sono ue) - "besides, in addition" (emphasizes importance)
  '彼は真面目だ。その上位成績もいい。',

  // しかも (shikamo) - "moreover" (emphasizes surprising addition)
  'この店は安い。しかも美味しい。',

  // また (mata) - "also, again" (less formal, broader meaning)
  'また明日会いましょう。',
  '彼もまた行くそうです。',

  // ますます (masumasu) - "increasingly" (emphasizes ongoing process)
  '雨はますます激しくなった。',
  '日本語がますます上手になる。',

  // いっそう (issou) - "more, even more" (similar but different word)
  'いっそう努力が必要だ。',

  // なお (nao) - "further, still" (formal, often in written context)
  '詳細はなおお問い合わせください。',

  // 再び (futatabi) - "again, once more" (repetition after pause)
  '再び会える日を楽しみにしています。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
