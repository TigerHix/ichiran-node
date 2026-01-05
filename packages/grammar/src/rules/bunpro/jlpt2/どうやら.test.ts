import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どうやら.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the どうやら grammar rule
const negatives = [
  // どうも (dōmo) - different meaning ("thank you" or "quite/by all means")
  'どうもありがとうございます。',
  'どうもうまくいかない。',
  'どうも変な感じがする。',

  // どうも...ようだ pattern (similar but different grammar point)
  '彼はどうも試験に失敗したようだ。',

  // なんとなく (nantonaku) - "somehow, vaguely" (different adverb)
  'なんとなくいい予感がする。',
  'なんとなく彼が来ると思っていた。',

  // なにやら (naniyara) - "something or other" (noun/pronoun, not adverb)
  'なにやらを見つけた。',
  '彼はなにやら言い残した。',

  // どう (dō) as question word
  'どうやって行きますか。',
  'どうすればいいですか。',

  // やら (yara) as particle (and so on)
  '本やらノートやらを買った。',
  '泣くやら叫ぶやらの大騒ぎだ。',

  // Sentences with conjecture but NO どうやら
  '彼は来るらしい。',
  '今日は雨が降るみたいだ。',
  '彼女は忙しいようです。',

  // どう or やら appearing separately
  'どうしようか迷っている。',
  '本やら雑誌やらを読む。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
