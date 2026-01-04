import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なくてもいい.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Positive てもいい forms (permission to do, not permission to not do)
  '行ってもいい。',
  '行ってもいいです。',
  '食べてもいい。',
  'してもいい。',
  '飲んでもいい。',
  '来てもいい。',
  '見てもいい。',
  '話してもいい。',
  // Simple negative なくて without もいい
  '行かなくて、',
  'しなくて、',
  '食べなくて、',
  // ないで forms (different negative pattern)
  '行かないで。',
  'しないで。',
  '食べないで。',
  '飲まないで。',
  // Simple negation (ない)
  '行かない。',
  'しない。',
  '食べない。',
  // なくてはいけない (opposite meaning - must do)
  '行かなくてはいけない。',
  'しなくてはいけない。',
  // Positive te-forms for other purposes
  '行って。',
  '食べて。',
  'しています。',
  '行っています。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
