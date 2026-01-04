import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ようだ.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples - sentences that should NOT match
const negatives = [
  // ように (in order to/so that) - different grammar
  '彼に会えるように早く行った。',

  // ような (like/similar to) - adnominal form modifying noun
  '彼のような人になりたい。',

  // そうだ (hearsay) - different auxiliary
  '彼は来るそうだ。',

  // らしい (seems like - hearsay) - different grammar
  '彼は来ないらしい。',

  // みたいだ (casual "seems like") - different word
  '彼は来ないみたいだ。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
