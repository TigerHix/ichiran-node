import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './causative-passive.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match causative-passive
const negatives = [
  // Causative form (not passive)
  '母は子供に野菜を食べさせた。',
  '先生は学生に本を読ませる。',
  // Passive form (not causative)
  '私は犬に噛まれた。',
  '彼は先生に褒められた。',
  // Potential form (different grammar)
  '私は日本語が話せる。',
  'この列車は速く走れる。',
  // Simple verb conjugations
  '私は昨日本を読んだ。',
  '彼は来月日本に行く。',
  // Causative without passive
  '父は私に宿題をさせた。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
