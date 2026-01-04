import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './しか-ない.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Number + しか + negative (different grammar point: Number + しか〜ない)
  // This pattern emphasizes small quantity rather than exclusivity
  'このテレビはチャネルが二つしかないけど、テレビをあまり見ないからいい。',
  '犯人はまだ３人しか捕まっていない。',
  '手袋を一枚しか見つけられない。',
  '釣りに行って、一匹しか釣れないと悲しくなる。',

  // だけ instead of しか (different grammar point)
  'これだけがある。',
  'ここだけにある。',

  // Positive verb (しか requires negative)
  // Note: This is grammatically incorrect in Japanese, but testing that we don't match it
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
