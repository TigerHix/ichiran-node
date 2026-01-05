import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たって.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Simple quoting/hearsay って (different grammar)
  '彼は来るって言ってた。',
  '明日は雨だって。',
  // Causative て-form + って
  '行ってって言った。',
  // だって meaning "because" or "but" (different grammar)
  'だって遅刻したんです。',
  'だってお腹が空くから。',
  // だって meaning "even/also" (different grammar)
  '私だってできるよ。',
  '子供だってわかる。',
  // Simple te-form + 何でも
  '食べて何でもする。',
  // Past tense without conditional meaning
  '彼は言った。それだけだ。',
  '昨日は買った。高いけど。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
