import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-て-b.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // て at end of sentence (request, not sequence)
  'もっと待って。',
  '少し待って。',
  'こっち来て。',
  '早く来て。',
  // て form for continuous action (ている)
  '何をしてるの？',
  '彼は今寝ています。',
  '雨が降っている。',
  // て form for state (te + aru/iru)
  '壁に絵が掛けてある。',
  'ドアが開いている。',
  // Negative test: single action with て (not connecting)
  '本を読んで。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
