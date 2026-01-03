import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './adjective-て-noun-で.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Different grammar: と for "and" (listing particle)
  '本とペンを買った。',
  'りんごとバナナを食べた。',

  // Different grammar: も for "also/too"
  '私も学生です。',
  '彼も来るでしょう。',

  // Conjunction だ (not te-form)
  '彼は学生だ。',
  '今日は晴れだ。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Noun/na-adjective + で conjunction (copula te-form)
//
// GiNZA parses this pattern inconsistently:
//   便利で     → pos=ADJ, lemma=だ (copula) ✓ WORKS
//   暇で       → pos=NOUN, lemma=だ (copula) ✓ WORKS
//   静かで     → pos=ADJ, lemma=だ (copula) ✓ WORKS
//   好きで     → pos=ADJ, lemma=だ (copula) ✓ WORKS
//   綺麗で     → pos=ADJ, pos=ADP, lemma=だ ✓ WORKS
//   きれいで   → pos=ADJ, pos=ADP, lemma=だ ✓ WORKS
//   病気で     → lemma=で (particle) ✗ INDISTINGUISHABLE
//   仕事で     → lemma=で (particle) ✗ INDISTINGUISHABLE
//   嫌いで     → lemma=で (particle) ✗ INDISTINGUISHABLE
//
// The discriminator `lemma=だ` identifies copula で vs locative で.
// But GiNZA only assigns lemma=だ to SOME noun/na-adjective+で conjunctions.
// When lemma=で, it's identical to locative で (東京で働く) or instrumental で (鉛筆で書く).
//
// Matching all で with lemma=で would overcapture:
//   ❌ 東京で働く (locative: work IN Tokyo)
//   ❌ 鉛筆で書く (instrumental: write WITH pencil)
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
// See also: adjective-て-b.ts which has the same issue.
const skipPositives = [
  '病気 → びょうきで',
  '嫌い → きらいで',
  '仕事 → しごとで',
  '音楽を作るのは仕事で趣味だ。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
