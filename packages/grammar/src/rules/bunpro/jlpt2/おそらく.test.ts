import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おそらく.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the おそらく grammar rule
const negatives = [
  // たぶん (tabun) - less formal, everyday usage
  'たぶん明日は雨でしょう。',
  'たぶん彼は来ないと思う。',

  // もしかしたら (moshikashitara) - lower certainty/speculation
  'もしかしたら雨が降るかもしれない。',
  'もしかしたら彼女は知っている。',

  // まさか (masaka) - expresses disbelief
  'まさか彼が犯人だとは思わなかった。',
  'まさかそんなことがあるわけがない。',

  // ぜったい/ぜったいに (zettai/zettai ni) - absolute certainty
  'ぜったいに成功する。',
  'ぜったい勝つ。',

  // どうやら (douyara) - "it seems, apparently" (based on observation)
  'どうやら雨が降りそうだ。',
  'どうやら彼は知らなかったようだ。',

  // なんとなく (nantonaku) - "somehow, vaguely" (vague feeling)
  'なんとなく雨が降りそうな気がする。',
  'なんとなく彼が来ると思った。',

  // おそらく as part of a compound (not the adverb)
  // Very unlikely to occur in natural text, but keeping as edge case

  // Similar sounding words
  // おそ (oso) - fear/danger (noun)
  '彼におそを感じる。',

  // おそる (osoru) - to fear (verb)
  '彼は失敗をおそれている。',

  // Sentences where おそらく is used but not as an adverb
  // Extremely rare/unnatural, but keeping for completeness
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
