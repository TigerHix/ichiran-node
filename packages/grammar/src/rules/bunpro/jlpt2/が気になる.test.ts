import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './が気になる.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  '彼のことを気にするな。',
  'そんなことを気にしないで。',
  '細かいことを気にしすぎる。',
  '何か変な気がする。',
  '雨が降る気がする。',
  '私は間違いに気がついた。',
  'その服が気に入った。',
  'もうすぐ春になる。',
  '彼は大人になる。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
