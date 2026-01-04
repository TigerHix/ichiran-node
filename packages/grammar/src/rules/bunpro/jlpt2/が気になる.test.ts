import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './が気になる.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // 気にする (to actively worry about - volitional)
  '気にしないでください。',
  '気にしすぎです。',
  // 気がつく (to notice - different grammar)
  '気がつかなかった。',
  'に気がつく。',
  // 気に入る (to like/please - different grammar)
  '気に入った。',

  // 気をつける (to be careful - different grammar)
  '気をつけて。',
  '気をつけてください。',

  // が気になる without proper topic structure
  'これは気になる。',  // Uses は instead of が

  // Similar but different patterns with なる
  'よくなる。',      // Just "to become better"
  '強くなる。',     // Just "to become stronger"
  '大きくなる。',   // Just "to become bigger"

  // ～に気をつける (to be careful about - different grammar)
  '健康に気をつける。',

  // ～気がする (to have a feeling - different grammar)
  '何か気がする。',
  '来る気がする。',

  // Noun + に + なる (simple becoming - unrelated)
  '春になる。',
  '夜になる。',
  '二十歳になる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
