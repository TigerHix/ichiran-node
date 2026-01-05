import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そういえば.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the そういえば grammar rule
const negatives = [
  // Similar discourse markers (different grammar)
  'ところで、昨日は何をしてた？',
  'ちなみに、明日は休みです。',
  'そうすると、彼も来るだろう。',
  'それにしても、遅いですね。',
  'それはそうと、昼ご飯食べた？',

  // といえば (to ieba) - "speaking of X" (requires noun topic)
  '夏といえば花火だね。',
  '日本といえば富士山を思い浮かべる。',
  '彼女といえば、最近連絡がない。',

  // そう alone (adverb "so/like that")
  'そう思います。',
  'そうしましょう。',
  'そうですね。',

  // いう (iu) - "to say" (different context)
  '彼はそう言った。',
  '何と言っていますか。',

  // いえば (ieba) - conditional form of "iu" alone
  'そう言えば本当だ。',
  '誰が言ったか言えば、彼だ。',

  // ば alone (conditional particle)
  '行ければ行きます。',
  '雨が降れば中止だ。',

  // Similar sounding but unrelated patterns
  'そういえばありません。',
  'そう言えば違います。',

  // Other similar conjunctions/discourse markers
  'その点では、彼が正しい。',
  'そのために、準備が必要だ。',
  'その結果、失敗した。',
  'さらに、詳細を説明します。',
  'なお、調査を続けます。',
  'また、明日も雨だ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
