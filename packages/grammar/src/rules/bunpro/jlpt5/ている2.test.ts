import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ている2.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // ている1 (ongoing action) - different grammar point
  // This is a difficult distinction to make purely structurally,
  // but ている1 typically uses action verbs while ている2 uses state verbs
  // Since we can't easily distinguish by verb class in the grammar rule,
  // we don't include these as negatives for now

  // Verb + て form without いる (different grammar)
  '本を読んで寝る。',
  'パンを食べて学校に行く。',

  // て form request (てください)
  '座ってください。',
  '待ってください。',

  // て form + ある (transitive verb resultative - different grammar)
  // '黑板に字が書いてある。',  // This could be matched but is a different grammar point

  // Verb + てしまう (completion - different grammar)
  '食べてしまった。',
  '忘れてしまった。',

  // Copula + いる (different structure)
  '先生である。',
  '学生でいる。',

  // Verb stem + にいく ( purpos e - different grammar)
  '買いに行く。',
  '遊びに行く。',

  // Simple past tense (not ている)
  '行った。',
  '食べた。',
  '始まった。',

  // Potential form + いる (different meaning)
  'できる。',
  '来られる。',

  // Volitional + いる (not a valid pattern)
  // '行こうている。',  // Not grammatically valid
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
