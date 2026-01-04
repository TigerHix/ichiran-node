import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './と同じくらい.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // 同じ by itself (without くらい) - just means "the same", not "about the same"
  'これはそれと同じです。',

  // 同じように - different grammar (similarity of manner, not degree)
  '彼と同じように勉強します。',

  // 同じような - different grammar (similar type/kind, not degree)
  '同じような問題が起きました。',

  // ほど - different grammar (used for degree/amount estimates)
  'それは想像するほど難しくなかった。',

  // と同時に - different grammar (at the same time)
  '私は彼と同時に到着しました。',

  // だけ同じ - not the target grammar
  '彼とだけ同じ意見です。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
