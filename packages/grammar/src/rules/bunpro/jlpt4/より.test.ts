import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './より.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // による (due to / by means of - different grammar)
  '地震による被害は大きかった。',
  'この製品は新型技術による改良が施されている。',

  // から (from - different particle)
  '東京から行きます。',
  '朝から晩まで働く。',

  // まで (until - different particle)
  '明日までに終わらせてください。',
  'ここまで来れば安心だ。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
