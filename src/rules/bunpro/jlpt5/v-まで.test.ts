import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './v-まで.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Noun + まで (different grammar point: noun-まで)
  '３時まで勉強します。',
  '日曜日まで待ちます。',
  '東京まで行きます。',
  // Place + まで (destination, not "until")
  '駅まで送ります。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
