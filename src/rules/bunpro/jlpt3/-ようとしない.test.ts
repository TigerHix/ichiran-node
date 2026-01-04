import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './-ようとしない.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ようとした (past, completed attempt) - different meaning
  '彼は逃げようとした。',
  'ドアを開けようとしたが、鍵がかかっていた。',
  // ようとしている (ongoing attempt) - different aspect
  '彼女は立ち上がろうとしている。',
  // Separate しない not connected to volitional
  '掃除はしない。勉強しようかな。',
  // ようにしない (manner) vs ようとしない (attempt)
  '遅刻しないようにしている。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
