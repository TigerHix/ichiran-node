import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たなり-なり.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Simple past tense (た) without なり - different grammar
  '彼は言った。それだけだ。',
  '昨日は買った。高いけど。',

  // Verb + たり (tari) pattern - "doing things like X and Y"
  '行ったり来たりしている。',
  '本を読んだり音楽を聴いたりする。',

  // Verb + だけ (dake) pattern - "only"
  '行っただけです。',
  '食べただけでいい。',

  // Noun + なり (nari) meaning "in X's way/style" - different grammar
  '彼なりに頑張っています。',
  '私なりのやり方でやってみたい。',

  // Noun + だ + なり meaning "as X" - copula usage, not たなり
  '学生なりに勉強する。',
  '彼は先生なりに指導した。',

  // Verb + て + なり (te-form + nari) - different from ta-form pattern
  // This would be て-form conjunctive + nari as conjunction, not past tense
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
