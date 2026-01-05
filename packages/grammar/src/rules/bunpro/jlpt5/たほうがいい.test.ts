import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たほうがいい.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Verb + た + ほう (comparison, not advice)
  'したほうが楽だ。', // doing is easier (comparison)
  // Verb + ほうがいい without た (different grammar - would be ば or たら)
  'すればいい。', // just do it (different pattern)
  // ほうがいい with ない-form (negative advice - different grammar rule)
  'あまり飲まないほうがいい。', // had better not drink (ないほうがいい)
  // Verb + た (simple past, not advice)
  '昨日、本を読んだ。', // read a book yesterday (past tense)
];


describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
