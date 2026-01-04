import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './number-も.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Inclusive も (also/too) - different grammar point
  // も following pronouns indicates "also/too", not number emphasis
  '私も行きたい。',
  '彼も来るでしょう。',
  'これも好きです。',
  '東京にも行きました。',
  '彼女も日本語が話せます。',
  '私も食べたいです。',

  // も following regular nouns (not numbers/counters)
  '子供も大好きです。',
  '犬も可愛いですね。',
  '先生も来ました。',

  // Topic particle は vs emphatic も
  // １０時間は (at least 10 hours) vs １０時間も (as long as 10 hours!)
  // These are different particles with different nuances

  // Other particles not related to number emphasis
  '時間がない。',
  'お金を持っている。',

  // Noun + も where も is the subject marker (inclusive)
  // Not number emphasis, just listing items
  'りんごもみかんも好きです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
