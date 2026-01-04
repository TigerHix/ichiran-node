import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verbて-b2.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: te-form used for other purposes (not reasons/causes)
const negatives = [
  // て at end of sentence (request/command, not reason)
  'もっと待って。',
  'こっち来て。',
  '早く来て。',

  // て form for continuous action (ている)
  '何をしてるの？',
  '彼は今寝ています。',
  '雨が降っている。',

  // て form for state (てある)
  '壁に絵が掛けてある。',
  'ドアが開いている。',

  // て form for completed action (てしまう - different grammar)
  '食べてしまった。',

  // て form for trying (てみる - different grammar)
  '食べてみます。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
