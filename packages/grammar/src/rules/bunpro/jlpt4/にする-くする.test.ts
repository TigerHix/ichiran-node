import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './にする-くする.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // Simple adverbial use (not "make X" construction)
  'よく勉強してください。',  // "study well" - adverbial use, not "make well"
  '静かに歩いてください。',  // "walk quietly" - adverbial use
  '早く起きなさい。',  // "wake up early" - adverbial use

  // に as direction/particle (not adverbial form)
  '東京に行きます。',  // "go to Tokyo" - directional ni
  '日本に住んでいます。',  // "live in Japan" - locative ni
  '3時に会いましょう。',  // "meet at 3" - temporal ni

  // する as main verb (not causative)
  '勉強する。',  // "to study" - suru as main verb
  '掃除する。',  // "to clean" - suru as main verb

  // Adjective predicate (not followed by suru)
  'この部屋は広いです。',  // "this room is spacious"
  '彼は優しい人です。',  // "he is a kind person"
  'この店は便利だ。',  // "this shop is convenient"

  // Na-adjective with copula (not suru)
  '彼は静かだ。',  // "he is quiet"
  'この部屋は綺麗です。',  // "this room is clean"
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
