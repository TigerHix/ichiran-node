import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かのようだ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative examples - sentences that should NOT match
const negatives = [
  // Regular ようだ (without かの) - different grammar, less uncertain
  '彼は来るようだ。',
  '彼は来ているようです。',
  '彼のような人になりたい。',

  // ように (in order to/so that) - different grammar
  '彼に会えるように早く行った。',

  // みたいだ (casual "seems like") - different word
  '彼は来ないみたいだ。',

  // そうだ (hearsay) - different auxiliary
  '彼は来るそうだ。',

  // らしい (seems like - hearsay) - different grammar
  '彼は来ないらしい。',

  // かだけ (different particle usage)
  'それかだけ知らない。',

  // のよう (without か)
  '彼女は子供のようだ。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
