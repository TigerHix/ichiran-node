import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './いきなり.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar adverbs that should NOT be matched
  // 急に (kyuu ni) - similar meaning but different word
  '急に雨が降ってきた。',

  // 突然 (totsuzen) - similar meaning but different word
  '突然、彼が現れた。',

  // いき (iki) as noun/prefix (not the adverb いきなり)
  // This would need context to be truly negative
  // 'いき' alone shouldn't match as it's incomplete

  // Sentences with いき but not いきなり
  // Note: These are rare, mostly いき appears in compounds
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
