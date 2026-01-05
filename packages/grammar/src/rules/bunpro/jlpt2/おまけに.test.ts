import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おまけに.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the おまけに grammar rule
const negatives = [
  // おまけ (omake) - noun meaning "freebie, bonus" alone (not the conjunction)
  'これはおまけだ。',
  'おまけがついてくる。',
  'サービスのおまけがあります。',

  // に (ni) - particle alone (not part of おまけに)
  '公園に行きます。',
  '友達に会いたい。',

  // Similar conjunctions with different meanings
  // その上 (sono ue) - "moreover" (formal, neutral)
  '彼は優秀だ。その上、性格もいい。',
  '顔が青白い。その上、唇は紫だ。',

  // それに (sore ni) - "and besides" (neutral, less formal)
  '彼は優秀だ。それに性格もいい。',
  'この店は安い。それに美味しい。',

  // しかも (shikamo) - "moreover" (less formal, emphatic)
  'この店は安い。しかも美味しい。',
  '彼は頭がいい。しかも親切だ。',

  // 更に (sara ni) / さらに - "further still" (progression/escalation)
  'さらに悪い結果になった。',
  '電気代がさらに高くなった。',

  // なお (nao) - "furthermore" (neutral, formal, simple addition)
  '詳細はなおお問い合わせください。',
  'なお、来月から値上げされます。',

  // 上に (ue ni) - "in addition to" (prepositional, within sentence)
  'このパソコンは安い上に性能もいい。',
  '彼は親切な上に、頭もいい。',

  // おまけ (noun) + different particle (not に)
  'おまけのサービスがあります。',
  'おまけとしてこれをもらった。',

  // Similar sounding but different words
  // お前 (omae) - "you" (informal)
  'お前は何をしているのか。',

  // 負ける (makeru) - "to lose" (verb)
  '試合に負けてしまった。',
  '彼に負けるのは嫌だ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
