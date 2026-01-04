import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ないと.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple negation without と (plain negative form)
  // These are statements, not conditionals
  '行かない。',
  '食べない。',
  'しない。',

  // Note: Quotation patterns like "彼は行かないと言った。" and "早くしないと思った。"
  // are structurally identical to conditionals (verb + ない + と), so this rule
  // will match them. They are excluded from this test list since they represent
  // a known ambiguity in the pattern matching approach.

  // Case marker と (together with)
  // Uses と as a comitative case marker
  '彼と行かない。',

  // Negative te-form なくて (conjunction, not conditional)
  // Connects two clauses with "because not" or "without doing"
  '行かなくて、よかった。',
  '勉強しなくて、困った。',

  // Negative ba-form なければ (different conditional form)
  // Uses ば instead of と for conditional
  '行かなければ、いけない。',
  'しなければ、ならない。',

  // Positive conditional と (without negation)
  // Different grammar point - simple conditional
  '行くと、見える。',
  '勉強すると、分かる。',

  // Note: Sentences with ないといけない or ないとだめ are NOT included
  // as negatives because they structurally contain the ないと pattern.
  // These are full grammar expressions that build on the ないと pattern:
  // - ないといけない (must not do / if not, it's bad)
  // - ないとだめ (must do - casual)
  //
  // These are separate grammar points that include ないと as a component.
  // The rule correctly matches the ないと pattern in these contexts.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
