import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それでも.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // それ alone (demonstrative pronoun "that", not the conjunction)
  'それは私の本です。',
  'それを持っている人は誰ですか。',
  'それを見せてください。',
  // でも as "but" at beginning of sentence (without それ)
  'でも、高いから買えない。',
  'でも、今日は行けません。',
  // で + も as separate particles (locative "at" + "also/even")
  '東京でも雨が降っています。',  // "Even in Tokyo, rain is falling"
  '図書館でも勉強します。',       // "I study even at the library"
  '彼でもできると思います。',     // "I think even he can do it"
  // でも as "however" (without それ prefix)
  '暑いです。でも、行きます。',
  '雨が降っています。でも、外出します。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
