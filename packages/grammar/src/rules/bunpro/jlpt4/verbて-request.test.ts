import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verbて-request.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Verbて for connecting clauses (sequential actions), not requests
  '駅まで走っていってて...',  // Thinking/incomplete speech
  '約束を守れなくてすみません。',  // Reason clause with て-form
  'ご飯を食べて、学校へ行きました。',  // Sequential actions
  // て-form in middle of sentence
  '彼に行ってと言いました。',  // Quoted speech
  // Negative te-form (なくて) - different pattern
  '行けなくて残念でした。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Verb-te request in quoted/multi-clause contexts
//
// GiNZA parses these cases inconsistently with verb-te requests:
//   まって。→ verb has dep=root ✓ WORKS (simple sentence)
//   提出して。→ verb/te has NO dep=root ✗ INDISTINGUISHABLE (in quote)
//   待ってて。→ verb has dep=root ✓ WORKS (usually)
//   少しまってて。→ verb/te has NO dep=root ✗ INDISTINGUISHABLE (before continuation)
//
// The discriminator dep=root identifies sentence-final te-form requests.
// But GiNZA only assigns dep=root to te-forms in simple standalone sentences.
// When the te-form is in quotes or followed by another clause, neither the verb
// nor the te particle gets dep=root, making them indistinguishable from
// non-final te-forms (connecting clauses, auxiliary verbs, etc.).
//
// Matching all te-forms without dep=root would overcapture:
//   ❌ ご飯を食べて、学校へ行きました (sequential actions)
//   ❌ 本を読んでいる (progressive ている)
//
// CONCLUSION: No reliable discriminator for te-forms in complex clause structures.
// GiNZA limitation.
const skipPositives = [
  '先生が生徒に言う：「この宿題、明日までに提出して。」',
  '「そこで少しまってて。すぐに戻るから。」',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
