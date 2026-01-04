import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './なければいけない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple negative verb (not obligation)
  '行かない。',
  '食べない。',
  'しない。',

  // Negative verb in ba-form (without ikenai - different grammar)
  '行かなければ。',
  'しなければ。',
  '来ればいい。',

  // te-form + wa + ikenai BUT positive form (prohibition, not obligation)
  '行ってはいけない。',
  '食べてはいけない。',
  '入ってはいけない。',

  // Similar obligation forms (different grammar - handled by different rules)
  // なくてはいけない (JLPT5)
  '行かなくてはいけない。',
  '勉強しなくてはいけない。',

  // なければならない (JLPT4 - separate rule)
  '行かなければならない。',
  '勉強しなければならない。',

  // ないといけない (JLPT4 - separate rule)
  '行かないといけない。',
  '勉強しないといけない。',

  // なくてもいい (permission, not obligation)
  '行かなくてもいい。',
  '食べなくてもいい。',

  // Negative potential form without obligation
  'これは行けない。',
  'それはできない。',

  // Negative ba-form with different verbs (not iku/ikeru)
  '来ればわかる。',
  '見れば見るほど好きになる。',

  // Simple ba conditional (not obligation)
  '行けば行くほど遠くなる。',
  '食べれば食べるほどおいしい。',
];

// Note: Past tense form なければいけなかった is now supported by Pattern 5

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
