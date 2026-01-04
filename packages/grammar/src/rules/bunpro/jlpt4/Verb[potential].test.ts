import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './Verb[potential].ts';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Intransitive verbs ending in similar sounds but not potential form
  'この本はよく売れる。',  // Sells well (intransitive, not potential)
  '壁に帰れる絵が掛かっている。',  // A picture that can return (nonsense, testing false positive)
];

// Skip positives: sentences that contain potential forms but are testing
// a different grammar point (こと が できる)
const skipPositives = [
  '日本語を読むことができる。',  // Testing こと が できる pattern, not direct potential
  '運転することができます。',  // Testing こと が できます pattern, not direct potential
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
