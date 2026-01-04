import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たばかり.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // てばかり (does nothing but) - different grammar
  '彼はゲームをしてばかりいる。',
  '寝てばかりいないで、勉強しなさい。',
  // ばかり (only/just) without ta-form - not "just finished"
  'この仕事は今日ばかりやる。',
  'お金ばかりが大事だ。',
  // ta-form without ばかり - just past tense
  '昨日、本を買った。',
  'ご飯を食べた。',
  // ばか (stupid) - unrelated word
  '彼はばかだ。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
