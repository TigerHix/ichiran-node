import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てごらん.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences with similar-looking patterns that should NOT match
const negatives = [
  // てください (polite request, not honorific "try")
  '食べてください。',
  '見てください。',
  '読んでください。',
  '入力してください。',
  // てみる (try doing - different grammar, volitional not honorific)
  '食べてみる。',
  '見てみる。',
  '食べてみてください。',
  '見てみて。',
  // てみる + sentence continuations (not てごらん)
  '見てみて、どうですか。',
  '食べてみたら美味しかった。',
  // Simple て-form continuations (no ごらん)
  '本を読んで寝た。',
  '外を見て虹が出ている。',
  '手を貸してあげるから。',
  // Past tense verbs (no て-form)
  '本を読んだ。',
  '見た。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
