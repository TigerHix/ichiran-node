import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './に-frequency.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Time point に (at specific time, not frequency)
  '三時に会います。',
  // Direction に (to/destination, not frequency)
  '東京に行きます。',
  // Indirect object に (to someone, not frequency)
  '彼に本をあげた。',
  // Location に (at/in place, not frequency)
  '公園にいます。',
  // Purpose に (for the purpose of, not frequency)
  '買い物に行きます。',
  // Result に (becomes, not frequency)
  '親切になります。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
