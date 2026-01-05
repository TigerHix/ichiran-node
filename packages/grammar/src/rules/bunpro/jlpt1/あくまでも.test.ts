import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あくまでも.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Similar adverbs with でも that have different meanings
  // いつでも - means "anytime" (different grammar)
  'いつでも連絡してください。',
  'いつでも行くよ。',
  // どこでも - means "anywhere" (different grammar)
  'どこでも座れる。',
  'どこでも寝られる。',
  // なんでも - means "anything" (different grammar)
  'なんでも食べる。',
  'なんでもいいよ。',
  // どれでも - means "whichever" (different grammar)
  'どれでも選んでいい。',
  // だれでも - means "anyone" (different grammar)
  'だれでも入れる。',
  // できれば - means "if possible" (different grammar)
  'できれば明日来てください。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
