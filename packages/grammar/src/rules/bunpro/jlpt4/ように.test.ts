import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ように.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // ～のように (manner/similarity = "like X", not purpose)
  // This should be matched by different rules (e.g., noun + のように = "like noun")
  '子供のように遊ぶ。', // play like a child (manner, not purpose)
  '彼のように日本語を話す。', // speak Japanese like him (similarity)
  '夢のように美しい。', // beautiful like a dream (similarity)
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
