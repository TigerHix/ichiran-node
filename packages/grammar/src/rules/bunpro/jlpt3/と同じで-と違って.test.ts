import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と同じで-と違って.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple noun + と (quotative particle) - not comparison
  '「はい」と言った。',

  // Noun + として (as/in the capacity of) - different grammar
  '先生として教えている。',

  // Noun + と共に (together with) - different grammar
  '彼と共に行く。',

  // Noun + と同時に (at the same time) - different grammar
  '終わると同時に帰った。',

  // Simple と for "and" - conjunction
  'リンゴとバナナを買った。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
