import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './おそらく.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Similar adverbs (not おそらく)
  'たぶん彼は来るだろう。',
  'もしかしたら雨が降るかもしれない。',
  'ぜったいに行く。',
  // おそらく used as part of a different word or phrase
  '恐ろしい事件があった。',
  '恐れ入りますが、もう一度説明してください。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
