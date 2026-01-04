import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './るところだ.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // ているところ (in the middle of doing - different grammar)
  '今勉強しているところです。',
  '映画を見ているところだ。',
  'ご飯を食べているところです。',
  // たところ (just finished doing - different grammar)
  '今帰ったところです。',
  '仕事が終わったところだ。',
  '晩ご飯を食べたところでした。',
  // ていたところ (was in the middle of - different grammar)
  '本を読んでいたところだ。',
  '宿題をしていたところでした。',
  // ところ as physical place (spatial, not grammatical "situation")
  'あの場所に行ったことがある。',
  'ここは良い場所だ。',
  // ところ as "point/aspect" (different grammar)
  '重要なところを見逃した。',
  '一番大切なところは分かりました。',
  // Verb without ところ (simple verb, not the grammar pattern)
  '友達と会う。',
  '今から帰る。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
