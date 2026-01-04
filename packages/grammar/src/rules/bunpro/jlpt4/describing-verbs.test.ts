import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './describing-verbs.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // True adverbs (not productive forms from adjectives)
  'とてもおいしい。',
  'もっと勉強してください。',
  'ずっと待っています。',

  // Ni as direction/particle (not adverbial form)
  '東京に行きます。',
  '日本に住んでいます。',

  // Ni as time marker (not adverbial form)
  '3時に会いましょう。',
  '朝にジョギングをします。',

  // Ku as part of verb (not adverbial form)
  '書きます。',  // kakimasu (verb ending, not adverb)

  // Adjectives not used adverbially
  'この部屋は広いです。',
  '彼は優しい人です。',
  'この本は面白い。',

  // Na-adjective as predicate (not adverbial)
  '彼は静かだ。',
  'この店は便利です。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
