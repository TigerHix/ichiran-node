import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './くせに.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Similar conjunctions that shouldn't match
  // のに (neutral "although/though")
  '頭がいいのにやる気がない。',
  '雨が降っているのに、外で遊んでいる。',
  // が (neutral "but/however")
  'この店は美味しいが、高い。',
  '彼は忙しいが、手伝ってくれた。',
  // せいで (negative "because of/due to")
  '雨のせいで、試合が中止になった。',
  '彼が遅刻したせいで、みんな待たされた。',
  // にしては (judgment standard "considering")
  '彼は初心者にしては、よくやる。',
  'この料理は簡単にしては、豪華だ。',
  // ながらも (contrasting simultaneous states)
  '彼は貧乏ながらも、幸せそうだ。',
  '危険だと知りつつも、行った。',
];

const skipPositives = [
  // All sentences now match - no skips needed
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
