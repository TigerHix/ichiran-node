import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './い-adjectives.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // きれい (fake i-adj, actually na-adj ending in い)
  '水はきれい。',
  // 静かだ (na-adjective with だ)
  '部屋は静かだ。',
  // 静かです (na-adjective with です)
  '部屋は静かです。',
  // 嫌い (actually na-adj, despite ending in い)
  'バナナも嫌い。',
  // 形容詞-一般 that's not an い-adjective in grammar sense
  'いろいろな本',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: い-adjectives that GiNZA parses incorrectly
//
// GiNZA parses some い-adjectives incorrectly:
//   おおい → tag="感動詞-一般" (interjection), not "形容詞-一般" ✓ INCORRECT
//   つまらない → parsed as verb "つまる" + aux "ない", not adjective ✗ INCORRECT
//
// When tag="感動詞-一般" or parsed as verb+aux, we can't match using
// tag="形容詞-一般" constraint because GiNZA doesn't assign it.
//
// To match these cases, we would need to either:
//   1. Match by surface text ending in "い" → would overcapture な-adjectives like "きれい"
//   2. Match different tag/pos patterns → would match non-i-adjectives
//
// CONCLUSION: GiNZA limitation for these specific lemmas.
const skipPositives = [
  '今週は仕事がおおい。',
  'この本はつまらないです。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
