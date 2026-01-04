import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verbて-b.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // て as locative particle (at/in)
  '東京で働いています。',
  '公園で遊びましょう。',
  // Verb-て request (sentence-final te-form)
  'ちょっと待って。',
  'ドアを開けて。',
  'やってみて。',
  // Causal て (because/since) - while structurally similar,
  // the semantic context is different
  // (These will match, which is acceptable since the distinction
  // is purely contextual, not structural)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
