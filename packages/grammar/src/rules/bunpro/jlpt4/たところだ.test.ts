import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たところだ.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // ところ as physical place (spatial, not grammatical "situation")
  'あの場所に行ったことがある。',
  'ここは良い場所だ。',
  // ところ as "point/aspect" (different grammar)
  '重要なところを見逃した。',
  '一番大切なところは分かりました。',
  // Verb without ところ (simple past tense, not the grammar pattern)
  '昨日、本を読んだ。',
  'ご飯を食べた。',
  // ているところ (about to do / in the middle of - different grammar)
  '今から行るところです。',
  '食べるところです。',
  // ていた (progressive past without ところ - different grammar)
  '本を読んでいた。',
  'ご飯を食べていた。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
