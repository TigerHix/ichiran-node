import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './だって.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // んだって (different grammar point - sentence-final explanatory hearsay)
  // This has a different structure and meaning from standalone だって
  '彼女は来るんだって。',

  // たって (JLPT2 - "even though" / "even if")
  // Different grammar point with similar form
  '雨が降ったって行きます。',
  '高かったったって買います。',

  // という (called/named - different grammar)
  // This is citation form, not hearsay
  'それは恋という。',

  // だ as copula (different from だって particle)
  // Should not match standalone copula
  '彼は学生だ。',

  // で + って (instrumental で + quotation って, not だって)
  '鉛筆で書いたって誰も信じない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
