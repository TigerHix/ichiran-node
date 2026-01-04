import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と共に.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple と particle (quotative or comitative, different grammar)
  // "With" in the basic sense, not the formal と共に
  '友だちと遊びます。',
  '彼と話しました。',

  // と同時に (JLPT3) - "at the same time as" (different grammar)
  // While similar, と同時に emphasizes temporal simultaneity more neutrally

  // につれて (JLPT3) - "as...then" (different grammar)
  // Focuses on proportional change over time

  // と (particle) - "and" (JLPT5)
  'リンゴとバナナを買いました。',

  // 並んで (JLPT3) - "in line with" (different grammar)
  // Emphasizes equal footing rather than accompaniment
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
