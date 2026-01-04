import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './化する.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: Similar patterns that should NOT match
const negatives = [
  // Question particle か (not the 化 suffix)
  '何か食べる？',
  '誰か来ましたか。',
  'いつか行きたい。',
  'どこかにある。',

  // か as sentence-final question particle
  '明日は雨か？',
  '彼は学生か。',

  // Regular suru-verbs (without 化/か suffix)
  '勉強する。',
  '練習する。',
  '掃除をする。',

  // か followed by particles (not verb conjugations or の)
  '本か雑誌を読む。',
  '犬か猫を飼っている。',

  // Standalone か not after a noun/adj/verb
  'それはかどうか分からない。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
