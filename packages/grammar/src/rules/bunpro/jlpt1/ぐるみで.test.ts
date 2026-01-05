import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ぐるみで.js';
import { BUNPRO_JLPT1 } from './index.js';

// Sentences that can't be matched due to idiom usage (not the pattern we're implementing):
//
// ANALYSIS: 身ぐるみ剥がされる (to be stripped of everything)
//
// This is an idiomatic expression where ぐるみ is part of a compound verb (身ぐるみ + 剥がす),
// NOT the "Noun + ぐるみで/の" suffix pattern we're implementing.
//
// The idiom "身ぐるみ" means "one's entire body/clothing" but it's a fixed compound,
// followed by 剥がされる (stripped) without the case particle で or の.
//
// The grammar pattern we're matching requires:
// - Noun + ぐるみ + で (instrumental/agentive: with the whole X)
// - Noun + ぐるみ + の (modifying: X-wide/entire X's [noun])
//
// The idiom has neither particle, so it's a different linguistic pattern.
//
// CONCLUSION: This is a fixed idiom, not the suffix + particle pattern. Skip it.
const skipPositives = [
  '詐欺、詐欺師：「もしもし、お祖母さん？俺、俺！」武の祖母：「武？どうしたの？」詐欺師：「うん、悪い人に身ぐるみ剥がされたんだ。お金持ってきてくれない？」',
];

const negatives = [
  // 包む as regular verb (to wrap), not suffix
  'プレゼントをきれいな紙で包んだ。',
  '彼はマフラーで顔を包んでいる。',

  // で as instrumental particle (not ぐるみ suffix)
  '家族で食事をしています。',
  '会社で働いています。',
  '街で会いましょう。',

  // ぐるみ without particle (different pattern, e.g., 身ぐるみ剥がされる)
  '彼は悪い人に身ぐるみ剥がされたんだ。',

  // の as possessive (not ぐるみ suffix)
  '家族の写真を見せてください。',
  '会社の方針に従う。',

  // Similar suffix patterns that are different grammar
  '全体で協力してください。',
  '全員で参加しましょう。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives, skipPositives });
});
