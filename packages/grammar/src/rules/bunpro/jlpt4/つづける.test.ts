import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './つづける.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 続ける (kanji) means "to continue" as main verb, not auxiliary
  '勉強を続ける。',
  '勉強を続けた。',
  '走り続けたのは私です。', // When used as main verb, not auxiliary
  // Different verbs with similar surface forms
  '勉強をしている。',
  '走っている。',
  '見ている。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
