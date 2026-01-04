import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './Verb[passive].js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match passive voice
//
// The auxiliary れる/られる has multiple uses:
// - Passive (ukemi): 受け身 - action done to subject (THIS RULE)
// - Potential (kanou): 可能 - possibility/capability
// - Spontaneous (jihatsu): 自発 - happens naturally/spontaneously
// - Respectful (sonkei): 尊敬 - honoring someone
//
// Since these all use the same grammatical structure (auxiliary attaching to verb),
// and GiNZA doesn't distinguish them structurally, this rule matches ALL uses.
// This is intentional: the rule identifies the grammatical form (verb + れる/られる),
// not the semantic meaning.
//
// The Bunpro Verb[passive] grammar point includes examples showing that られる
// can express multiple meanings (passive, potential, etc.) with the same form.
// Therefore, this rule matches all of them.

const negatives = [
  // Causative form - different auxiliary (せる/させる)
  '子供に野菜を食べさせる。',
  '彼を行かせる。',

  // Causative-passive form - combined causative and passive
  '兄にケーキを食べさせられた。',
  '毎日勉強させられる。',

  // Simple verb forms without passive
  '本を読む。',
  'ご飯を食べる。',
  '日本へ行く。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
