import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './よていだ.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Similar expressions that should NOT match
  // つもりだ (intend to) - different grammar with stronger intention
  '明日は出発するつもりだ。',
  '日本に留学するつもりです。',

  // ことになっている (has been arranged) - different grammar structure
  '会議は月曜日ことになっています。',
  '試合は三時から始まることになっている。',

  // はずだ (should be/expected to be) - different grammar
  '彼が来るはずだ。',
  '試合が始まるはずです。',

  // だろう (probably) - different grammar
  '明日は晴れるだろう。',
  '彼が来るだろう。',

  // ようだ (seems like) - different grammar
  '雨が降るようだ。',
  '彼が来たようです。',

  // みたいだ (looks like) - different grammar
  '彼が来たみたいだ。',
  '雨が降るみたいです。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
