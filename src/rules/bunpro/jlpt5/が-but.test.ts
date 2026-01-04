import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './が-but.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: sentences where が is the subject marker, NOT conjunction.
// These have が with dep=case (subject particle), not dep=dep/cc (conjunction).
const negatives = [
  // Subject marker が - standard subject marking
  '私が行きます。',
  '彼が学生です。',
  '雨が降っています。',
  '猫が好きです。',
  '何がしたいですか。',
  // Object/experiencer が with potential verbs or adjectives
  '日本語がわかります。',
  '水が欲しいです。',
  'お金があります。',
  // Double が sentences (first is subject, second could be conjunction)
  // Note: We only test sentences where ALL が are subject markers
  '私が私が',  // Multiple subject markers (unnatural but grammatically valid for testing)
  '彼が彼女が好きだ',  // Both marking subjects
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
