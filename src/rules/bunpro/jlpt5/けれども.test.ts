import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './けれども.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative tests: casual conjunctions and other particles that should NOT match
const negatives = [
  // Casual けど (shortened form, not formal)
  '明日は雨だけど、出かけます。',
  'これは高いけど、買います。',
  '忙しいけど、手伝います。',
  // Casual だけど (after copula)
  'これは本だけど、面白い。',
  '今日は日曜だけど、働きます。',
  // が conjunction (more formal than けれども, but different pattern)
  '毎日走るが、運動はきらいです。',
  'お金は大切だが、時間も大切だ。',
  // のに (different conjunction: "even though")
  '雨が降っているのに、出かけます。',
  '勉強したのに、テストが悪かった。',
  // でも at beginning of sentence (different grammar)
  'でも、私は行きたいです。',
  'でも、大丈夫です。',
  // としても (different grammar point)
  '行ったとしても、会えません。',
  '高くても、買います。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
