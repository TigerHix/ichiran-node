import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からには.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // Regular から (without には/は) - not emphatic determination
  'あなたがいるから、安心できます。',
  '毎日努力をしたから、一番になれた。',
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  // から as source/origin (not reason)
  '日本から来ました。',
  '彼からもらった。',
  '昨日から雨が降っている。',
  // には without から (different grammar)
  '東京には行ったことがない。',
  '彼には話さないつもりだ。',
  // だけに (just/only) - different grammar
  '努力しただけに、結果が出た。',
  '高かっただけに、期待している。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
