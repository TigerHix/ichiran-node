import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './verb-volitional-としたが.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ようとする (present tense, no conjunction) - different meaning
  '彼は逃げようとする。',
  'ドアを開けようとするが、鍵がかかっている。',
  // ようとしている (ongoing attempt) - different aspect
  '彼女は立ち上がろうとしている。',
  // ようとしない (negative) - different rule
  '彼は逃げようとしない。',
  // Separate した not connected to volitional
  '宿題をした。勉強しよう。',
  // Simple volitional without とする
  '勉強しよう。',
  '一緒に行こう。',
  // ようにする (manner/habit) vs ようとする (attempt)
  '毎日運動するようにしている。',
  '遅刻しないようにしている。',
  // Simple past したが without volitional context
  '勉強したが、難しかった。',
  '彼は来たが、彼女は来なかった。',
  // ようとした (complete, no conjunction)
  '彼は逃げようとした。',
  'ドアを開けようとした。',
  // としたら (conditional) - different grammar point
  'もし雨が降ったら、行きません。',
  'そんなに高いとしたら、買わない。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
