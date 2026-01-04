import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './各.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // それぞれ (sorezore) - different grammar, can stand alone or use の
  '人はそれぞれ違う意見がある。',
  'それぞれの部屋を掃除してください。',
  // 各自 - related but different (means "each person individually")
  '各自で弁当を持ってきてください。',
  // 各々（おのおの）onoono - different word for people
  '弁当は各々（おのおの）で用意してきてください。',
  // 毎 - attached to time nouns, not general nouns
  '毎日学校に行きます。',
  '毎朝ジョギングをします。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Prefix 各/かく (each/every)
//
// GiNZA parses "かく" inconsistently in these sentences:
//   - このバスはかくバス停に止まります。 → PARSABLE ✓
//   - このエレベーターはかく階で止まります。 → NOT PARSED ✗ (same pattern)
//   - この旅館には日本かく地からお客さまが来る。 → NOT PARSED ✗
//   - ここは正月になると日本かく地からの観光客でいっぱいになる。 → NOT PARSED ✗
//
// The token "かく" appears in the test sentence text but GiNZA doesn't tokenize it
// as a separate token in these 3 specific cases. This appears to be a GiNZA parsing
// inconsistency where certain compounds (日本かく地, かく階+で) are parsed differently.
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  'ここは正月になると日本かく地からの観光客でいっぱいになる。',
  'このエレベーターはかく階で止まります。',
  'この旅館には日本かく地からお客さまが来る。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
