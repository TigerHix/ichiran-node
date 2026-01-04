import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たら.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // かったら without being a past adjective (different grammar - かったら as surprise discovery)
  // This is actually a positive case for たら, so skip
  // 'あ、千円だったら、買えますよ。', // This is a valid たら pattern

  // Simple verb in past tense without ら
  '昨日勉強した。',

  // Simple i-adj in past tense without ら
  '昨日は寒かった。',

  // Simple noun+だった without ら
  '昨日は日曜日だった。',

  // ら as part of other grammar (not conditional)
  // Note: This is tricky - ら can appear in other contexts, but the test data
  // suggests we should match the pattern even if it could be ambiguous.
  // Let the rule match and rely on context.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
