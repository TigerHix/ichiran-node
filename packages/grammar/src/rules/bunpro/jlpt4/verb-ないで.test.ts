import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-ないで.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple negation without で (plain negative form)
  // These are statements, not requests
  '行かない。',
  '食べない。',
  'しない。',

  // Negative conjunction なくて (connects clauses, not a request)
  // This has a different meaning: "because X didn't happen, Y..."
  '行かなくて、よかった。',
  '食べなくて、勉強した。',
  '言わなくて、黙っていた。',

  // Positive te-form request て (different grammar)
  '行って。',
  '食べて。',

  // Plain te-form for conjunction (different grammar)
  '行って、買ってきた。',
  '食べて、寝た。',

  // Independent ない (negation without で)
  '彼は来ない。',
  'お金がない。',

  // て + negative (ては - different grammar)
  '食べてはいけない。',
  '行ってはだめ。',

  // て-form positive (て - different grammar)
  '行って。',
  '食べて。',

  // Note: The following are NOT included as negatives because they
  // structurally contain the ないで pattern, even though they represent
  // different pragmatic uses:
  //
  // - ないでください (polite form) - structurally contains ないで
  // - ないで ("without doing") - structurally contains ないで
  //
  // These are distinct grammar points (Bunpro IDs 65 and 96) that happen
  // to share the same surface form. The distinction is pragmatic/contextual,
  // not structural. This rule correctly matches the ないで pattern in all
  // these contexts; interpretation depends on the broader sentence context.
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
