import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でもある.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // でも meaning "even" with question words (different grammar)
  // These use で as case particle (ADP,dep=case) not copula (AUX,dep=cop)
  'どこでも行ける。',
  '何でも食べる。',
  '誰でも知っている。',
  'いつでも来てください。',

  // て-form verb + も (conditional "even if")
  // This uses て/で (SCONJ,dep=mark) + も (ADP,dep=case)
  'お金がなくても買える。',
  '雨が降っても行く。',
  '高くても買う。',

  // Similar patterns that should NOT match
  'それは彼でもない。',
  '私でもできる。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
