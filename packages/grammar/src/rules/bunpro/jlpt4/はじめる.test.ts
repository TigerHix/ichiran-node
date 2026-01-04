import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './はじめる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 始める (kanji) means "to start" as main verb, not auxiliary
  '勉強を始める。',
  '勉強を始めた。',
  '会議を始めましょう。',
  '映画が始まる。',
  // Different verbs with similar surface forms
  '勉強をしている。',
  '走っている。',
  '見ている。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
