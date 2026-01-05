import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './といった.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the といった grammar rule
const negatives = [
  // という (toiū) - "called" or "known as" (not "such as")
  'これは何という花ですか。',
  '東京という都市は大きい。',
  '彼は田中という人だ。',
  'これは何という本ですか。',

  // など (nado) - "etc." or "and so on" (different pattern)
  'りんごやバナナなどが好きです。',
  '東京や大阪などに行きました。',
  '本や雑誌などを買った。',

  // とか (toka) - "things like" (lists examples separately)
  'りんごとかバナナとかを買った。',
  '映画とか音楽とかが好きです。',
  '東京とか大阪とかに行きたい。',

  // なんて (nante) - dismissive/emotional (seldom followed by noun)
  '彼なんて知りません。',
  'お金なんていらない。',
  'そんなことなんてできない。',

  // Similar but unrelated patterns
  // と言った (to itta) - quotative "said that"
  '彼は行くと言った。',
  'そう言ったのは彼です。',
  '彼女は好きだと言った。',

  // として (toshite) - "as" or "in the capacity of"
  '学生として参加する。',
  '彼は医者として働いている。',
  '友人として忠告する。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
