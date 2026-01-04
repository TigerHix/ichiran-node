import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かなり.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: similar adverbs that should NOT match
const negatives = [
  // Similar degree adverbs (different grammar points)
  // なかなか - also "quite" but with different nuance and usage patterns
  'なかなか美味しいですね。',
  'なかなか良い天気です。',

  // けっこう - "quite/considerably" but stronger nuance
  'けっこう遠いですね。',
  'けっこう高いです。',

  // だいぶ - "considerably" stronger than かなり
  'だいぶ疲れた。',
  'だいぶ寒いです。',

  // そうとう - "quite/to a considerable extent"
  'そうとう困っている。',

  // すごく - "very/extremely" (stronger degree)
  'すごく美味しい。',
  'すごく面白い。',

  // あまり - usually with negative (not very)
  'あまり美味しくない。',

  // あまりに - "too/overly" (expresses surprise/shock about degree)
  'あまりに痛い。',
  'あまりに多い。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
