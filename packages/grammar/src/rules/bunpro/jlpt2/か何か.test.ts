import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './か何か.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  '面白いかどうか分からない。',
  '行くかどうかまだ決めてない。',
  '雨が降るかも。',
  '明日は晴れるかも。',
  '彼が遅れたかというと、電車が遅れたからだ。',
  '何か食べたい。',
  '何かあったの？',
  'お茶でも飲みませんか？',
  'りんごとかバナナとか買った。',
  '明日来るか？',
  '何色が好き？',
  '明日は雨かな。',
  '賛成しかねる。',
  '火事になりかねない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
