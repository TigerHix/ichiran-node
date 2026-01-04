import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './でも-でも.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // それでも (conjunction: even so, nevertheless - different grammar)
  'このコーヒーは甘いですが、それでも砂糖を加えます。',
  'タバコは体に悪いと言われている。それでも、止めにくい。',
  '高いのに、それでも買うつもりですか。',

  // Single でも (instrumental means: with/using)
  '電車で行きます。',
  '日本語で話してください。',
  '鉛筆で書きます。',

  // でも as contrastive conjunction (but/however)
  '雨が降っています。でも、私は出かけます。',
  '彼は忙しい。でも、手伝ってくれます。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
