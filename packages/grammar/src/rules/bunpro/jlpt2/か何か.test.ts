import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './か何か.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the か何か grammar rule
const negatives = [
  // Simple 何か (something) without preceding noun + か
  '何か食べたい。',
  '何か困ったことがありましたか。',
  '何かいいことがありそう。',
  '何か質問はありますか。',

  // かどうか (whether or not) - different grammar
  '面白いかどうか分からない。',
  '行くかどうかまだ決めてない。',
  'できるかどうかわかりません。',

  // か as question particle (not part of か何か)
  'これは何ですか？',
  '行きますか？',
  '好きですか？',

  // でも (or something) - different grammar, more vague
  'お茶でも飲みませんか？',
  'コーヒーでも飲みたい。',
  '何でもいいです。',

  // か alone as "or" between choices
  '紅茶かコーヒーを選んでください。',
  '行くか行かないか、決めてください。',
  '朝か夜に会いましょう。',

  // Question word + か (some/someone)
  '誰か来ました。',
  'どこかで会った気がする。',
  'いつか行きたい。',

  // Noun + か + 何 (not 何か)
  'リンゴか何を買いましたか。',
  'これか何を食べたい？',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
