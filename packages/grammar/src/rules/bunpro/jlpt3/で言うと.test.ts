import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './で言うと.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Different grammar points that should NOT match:
  // から言うと (different particle, different grammar point)
  '私の経験から言うと、それは正しい。',

  // と言う (quotational と, different grammar)
  '彼は来ると言った。',
  '「こんにちは」と言う。',

  // ではない (copula negation, different grammar)
  'それは間違いではない。',

  // では (conjunction, different grammar)
  'ここでは食べられません。',

  // Simple で (instrumental/locative case marker, not part of で言うと)
  '鉛筆で書く。',
  '電車で行く。',
  '日本で勉強する。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
