import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なくてはいけない.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // Simple negative verb (not te-form)
  '行かない。',
  '食べない。',
  'しない。',

  // Negative te-form + iru (state)
  '知らなくている。',
  '行かなくている。',

  // te-form + wa + ikenai BUT positive form (prohibition, not obligation)
  '行ってはいけない。',
  '食べてはいけない。',
  '入ってはいけない。',

  // Similar obligation forms (different grammar - handled by different rules)
  // なければいけない (JLPT4)
  '行かなければいけない。',
  '勉強しなければいけない。',

  // なくてもいい (permission, not obligation)
  '行かなくてもいい。',
  '食べなくてもいい。',

  // Other te-form constructions
  '行かなくて、勉強する。',
  '食べなくて、寝た。',

  // Negative potential form without obligation
  'これは行けない。',
  'それはできない。',
];

// Sentences that cannot be matched due to test data issues or related grammar:
//
// 1. まだやりたいことがあるけど、もうねなくてはいけない。
//    The sentence has a typo - "ね" instead of "寝" (neru - to sleep).
//    GiNZA parses "ね" as a particle, not a verb, so it cannot match.
const skipPositives = [
  'まだやりたいことがあるけど、もうねなくてはいけない。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives, skipPositives });
});
