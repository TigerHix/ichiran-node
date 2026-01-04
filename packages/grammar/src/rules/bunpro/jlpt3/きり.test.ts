import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './きり.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // 切る (verb "to cut") - different meaning
  'ナイフでパンを切る。',
  'ケーブルを切ってはいけません。',
  // 切れる (intransitive "to break") - different meaning
  '糸が切れた。',
  // きり as standalone sentence particle not matching the pattern
  // (hard to construct a good negative for this)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
