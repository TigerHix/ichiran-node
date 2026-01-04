import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おそらく.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar adverbs with different nuances
  // たぶん - casual "probably" (higher likelihood)
  'たぶん明日は雨だろう。',
  'たぶん彼は来ない。',
  // もしかしたら - "perhaps, maybe" (lower likelihood)
  'もしかしたら雨が降るかもしれない。',
  'もしかしたら彼が犯人だ。',
  // まさか - "no way, can it be" (unexpected)
  'まさか彼が犯人だろう。',
  'まさか本当にそんなことがあったの？',
  // ぜったい(に) - "absolutely, definitely" (not "probably")
  'ぜったいに行く。',
  'ぜったいに勝つ。',
  // どうやら - "it seems, apparently" (based on observation)
  'どうやら明日は雨らしい。',
  'どうやら彼は知らないようだ。',
  // どうも - "somehow, very" (different nuance)
  'どうもありがとう。',
  'どうも変だ。',
  // おそらくish - similar sounding but different
  'おそるおそる手に取った。', // 恐る恐る "cautiously"
  'おそれがある。', // 恐れがある "there is a fear that"
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
