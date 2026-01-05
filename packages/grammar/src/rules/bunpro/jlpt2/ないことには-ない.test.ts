import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないことには-ない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the ないことには-ない grammar rule
const negatives = [
  // こと without には - different meaning
  'これは大切なことです。',
  '彼のことが心配です。',
  'いろいろなことがあります。',

  // ことには + affirmative (not negative) - different grammar
  // While the writeup mentions this can happen, the core pattern requires the second negative

  // ないで (without doing) - different grammar
  '傘を持たないで出かけた。',
  '昨日は勉強しないで寝た。',

  // ないまま (in the state of not being) - different grammar
  '靴を履かないまま出た。',
  '窓を開けないまま寝た。',

  // ないかぎり (unless) - similar but different structure
  '雨が降らないかぎり、試合は行われます。',
  '努力しないかぎり、成功できない。',

  // てからでないと (cannot unless/until) - different structure
  '詳しく見てからでないと、わかりません。',
  '確認してからでないと、承諾できません。',

  // Simple negative sentences without ことには
  '彼は来ない。',
  'お金がない。',
  '私は知らない。',

  // ことになる (it has been decided) - different grammar
  '来週会うことになった。',
  '出発することになった。',

  // ことにする (decide to) - different grammar
  '毎日運動することにした。',
  '早起きすることにする。',

  // ことになっている (supposed to/arranged) - different grammar
  '授業は9時に始まることになっている。',
  '禁煙ことになっている。',

  // Similar patterns with different particles
  // ことではない (not the case that) - different grammar
  '難しいことではない。',
  '特別なことではない。',

  // Negative + こと + different particle
  'ないことはないです。',
  'ないこともない。',

  // Simple sentences with あること
  '彼が来たことは確かです。',
  'そんなことがあったとは知らなかった。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
