import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そう.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // そうだ (hearsay - different grammar)
  // This attaches to plain forms, not stems
  '雨が降るそうだ。',
  '彼は来るそうだ。',
  '日本語が上手だそうだ。',

  // Conjunction そう (and then/so)
  'お金がないそう、買えません。',

  // そう as "so/that way" (adverbial)
  'そう思います。',
  'そうしましょう。',
  'そうしてください。',

  // そう as "yes" (confirmation)
  'そうですね。',

  // Adverbs ending in そう that aren't the grammar point
  // These are lexical items, not productive patterns
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
