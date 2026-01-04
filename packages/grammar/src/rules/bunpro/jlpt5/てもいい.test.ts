import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てもいい.js';
import { BUNPRO_JLPT5 } from './index.js';

// Sentences that can't be matched due to data quality issues:
//
// The てもいい grammar point is specifically for i-adjectives + て + も + いい,
// na-adjectives + で + も + いい, and nouns + で + も + いい.
//
// However, the Bunpro data incorrectly includes:
// - これ食べてもいいの？ (verb + てもいい)
//
// This is actually a verb + てもいい construction, which should be handled
// by the separate verb-てもいい grammar point. The grammar point definition
// clearly states it's for "［い］Adjective［く］+ ても + いい" and
// "［な］Adjective + でも + いい" and "Noun + でも + いい".
//
// CONCLUSION: Data quality issue - sentence belongs to verb-てもいい, not てもいい.
const skipPositives = [
  'これ食べてもいいの？',
];

// Negative tests: sentences that should NOT match this rule
const negatives = [
  // verb-てもいい should not match here (different rule)
  '行ってもいいですか。',
  '食べてもいい。',
  // Simple te-form without もいい
  '寒くて、眠い。',
  'これは安くていい。',
  // Locative では (case marker)
  '東京では雨が降っています。',
  '図書館では勉強します。',
  // Instrumental で (means/tool)
  '鉛筆で書く。',
  '日本語で話してください。',
  // ではない negation
  'これは本ではない。',
  // Conjunction でも (even/even if) without いい
  '雨でも行きます。',
  '子供でもできます。',
  // Just て form without もいい
  '寒くて大変です。',
  // て form for sequential actions
  '起きて、顔を洗う。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
