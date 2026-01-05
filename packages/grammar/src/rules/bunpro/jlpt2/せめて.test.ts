import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './せめて.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the せめて grammar rule
const negatives = [
  // すくなくとも (sukunakutomo) - more formal/neutral "at least"
  'すくなくとも一度は行ってみたい。',
  'すくなくとも100人は集まるだろう。',

  // どうせ (douse) - "anyway, in any case" (resignation)
  'どうせ無駄だ。',
  'どうせ遅刻するので、ゆっくり行く。',

  // どうやら (douyara) - "apparently, seemingly" (conjecture)
  'どうやら雨が降りそうだ。',
  'どうやら彼は知らなかったようだ。',

  // せめる (semeru) - "to blame, criticize" (verb)
  '彼をせめることはできない。',
  '自分をせめるべきだ。',

  // Similar sounding but unrelated words
  // せめて (semete) as part of other expressions
  // せめてもの (semete mono) - "the least/only thing"
  // Note: This IS actually a related use of せめて, so we shouldn't test against it
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
