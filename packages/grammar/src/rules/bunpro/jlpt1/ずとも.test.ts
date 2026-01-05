import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずとも.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // ず followed by different particle, not とも
  '彼は何も言わずに部屋を出て行った。',
  // ずとも but in different context (conjunction, not this grammar)
  // Note: Actually hard to construct false positives since pattern is unique
  // Similar but different grammar:
  '彼は知らず知らずのうちに眠ってしまった。', // 知らず知らず (idiom)
  '彼は泣かずに平気な顔をしていた。', // ずに (negative te-form)
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
