import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そういう.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // そう as adverb meaning "so" or "in that way" (not followed by いう + noun)
  'そう思う。',
  'そうしてください。',
  'そうすればいい。',
  // そう as auxiliary verb meaning "seems like"
  '雨が降りそうだ。',
  'おいしそうなケーキだ。',
  // こう meaning "like this" (not followed by いう + noun)
  'こう書いた。',
  'こうやってやる。',
  // どう meaning "how" (not followed by いう + noun)
  'どうするの？',
  'どうやって行く？',
  // Plain demonstrative + noun (without いう)
  'その人は嫌い。',
  'この人はいい人だ。',
  'あの人は誰ですか。',
  // いう as quotation particle (different grammar)
  '彼は行くと言った。',
  'はいと言ってください。',
  // Different い-adjective ending in いう (e.g., かっこいい)
  'かっこいい人だ。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
