import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たり-たりする.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Simple て form (different grammar - verb-て)
  'テレビを見て、寝る。',
  '勉強をして、掃除をした。',
  // Past tense verbs without り
  'テレビを見た。',
  '勉強をした。',
  // Dictionary form verbs (not past form)
  'テレビを見る。',
  '勉強をする。',
  // Verb stem + たり without final する
  'テレビを見たり、寝たり。',
  // Noun + たり (without suru-verb pattern)
  '本たり、ノートたり買う。',
];

const skipPositives: string[] = [];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
