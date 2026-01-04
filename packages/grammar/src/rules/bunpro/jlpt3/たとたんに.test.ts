import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たとたんに.js';
import { BUNPRO_JLPT3 } from './index.js';

// False positives: sentences with similar patterns that should NOT match
const negatives = [
  // Simple verb + た (past tense without とたん)
  '昨日、町に行った。',
  'ご飯を食べた。',
  // Verb + た + と (quotative, not とたん)
  '彼が行ったと言った。',
  '終わったと思う。',
  // た + と + separate noun (not とたん compound)
  '書いたと本を買った。',
  // Different grammar: たとたん vs たところ
  '帰ったところだ。',
  '食べたところです。',
  // Different grammar: たとたん vs たばかり
  '来たばかりです。',
  '買ったばかりの本。',
  // たとたん cannot follow nouns or adjectives (only verbs)
  // (These are included as negative test cases)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
