import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './がてら.js';
import { BUNPRO_JLPT1 } from './index.js';

// Negative examples: similar patterns that should NOT match
const negatives = [
  // ついでに - similar meaning but different word
  '買い物に行くついでに、郵便局に行ってこの荷物を出しておいてくれる？',
  '散歩のついでに寄って行ってください。',
  // ながら - similar "while doing" but different grammar
  '音楽を聴きながら勉強する。',
  'テレビを見ながらご飯を食べる。',
  '歩きながらスマホを見ないで。',
  // かたがた - similar "while doing" but different word
  // (Note: かたがた grammar doesn't exist in our test data yet)
  // をかねて - similar "doing X while also Y" but different word
  // (Note: をかねて grammar doesn't exist in our test data yet)
  // うちに - "while/during" with different meaning
  '寝ているうちに電話がかかってきた。',
  '若いうちにたくさん勉強しておいてください。',
  // 間に - "during/between" with different meaning
  '昼休みの間に昼寝をした。',
  '授業の間に眠ってしまった。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
