import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './からこそ.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Regular から (without こそ) - not emphatic
  'あなたがいるから、安心できます。',
  '毎日努力をしたから、一番になれた。',
  '東京から大阪へ行く。',
  '8時から仕事を始めます。',
  // から as source/origin (not reason)
  '日本から来ました。',
  '彼からもらった。',
  '昨日から雨が降っている。',
  // こそ without から (different grammar)
  '私こそが社長です。',
  '今こそ頑張るときだ。',
  // だか(ら) as "what" (slang) + こそ (different meaning)
  // This wouldn't naturally occur, but keeping as placeholder
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
