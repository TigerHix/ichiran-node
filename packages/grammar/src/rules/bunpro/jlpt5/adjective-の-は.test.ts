import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './adjective-の-は.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Possessive の (not nominalization)
  '私の犬です。',
  '彼の車だ。',
  'これは友達の本です。',
  // の for nominalization of verb phrases (different grammar)
  '読むのが好きです。',
  '泳ぐのは楽しいです。',
  // Just adjective + な (without の)
  'この部屋は静かです。',
  '彼女はきれいだ。',
  // Adjective as predicate without nominalization
  'このペンは高いです。',
  '今日は寒い。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
