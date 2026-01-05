import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ずにはすまない.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative test cases - sentences that should NOT match the ずにはすまない grammar rule
// These test similar patterns and related grammar
const negatives = [
  // ずに (zu ni) - "without doing" (different grammar, incomplete pattern)
  '彼は一言も謝らずに去った。',
  '傘を持たずに出かけた。',
  '何も言わずに部屋を出た。',

  // ないでは (nai dewa) - different context (not + では for emphasis)
  'これは試験ではない。',

  // には (ni wa) - topic marker combination (different grammar)
  '東京には行ったことがある。',
  '私には難しい問題だ。',

  // では (de wa) - conjunction or locative (different grammar)
  '図書館では静かにしてください。',
  'ここでは喫煙禁止です。',

  // ずにしては (zu ni shite wa) - "considering that..." (different grammar)
  '初心者にしてはよくやっている。',

  // にしては (ni shite wa) - "considering that..." (different grammar)
  '彼にしては簡単な問題だ。',

  // ずにはいられない (zu ni wa irarenai) - "cannot help but do" (different grammar)
  '泣かずにはいられない。',
  '笑わずにはいられない。',

  // ないわけにはいかない (nai wake ni wa ikanai) - "must do" (different grammar)
  '明日は試験があるから、勉強しないわけにはいかない。',
  '約束を守るわけにはいかない。',

  // ざるをえない (zaru o enai) - "have no choice but to" (alternative grammar)
  '同意せざるをえない。',
  '謝罪せざるをえない。',

  // Simple negation with ない (different pattern)
  '謝らない。',
  '行かない。',
  'しない。',

  // すむ without negation (different verb)
  'それで問題はすむ。',
  '金ですむことではない。',

  // Verb + すむ (to finish/end) - different meaning
  '仕事がすんだ。',

  // ず without には (incomplete pattern)
  '謝らずに済んだ。',

  // Similar patterns with different endings
  '謝らずにはいけない。',
  '謝らないではいけない。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
