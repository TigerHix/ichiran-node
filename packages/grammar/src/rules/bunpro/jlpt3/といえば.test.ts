import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './といえば.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // という - "called" / "known as" (different grammar - naming/defining)
  // 東京という町 - "a town called Tokyo"
  // This is different from "speaking of Tokyo"
  '東京という町に行きたい。',

  // そういえば - "come to think of it" / "now that you mention it"
  // This is a different grammar point (JLPT2)
  'そういえば、明日は休みだった。',

  // と言っても - "even if I say" / "although I say" (JLPT3)
  // Different grammar point with concessive meaning
  '安いと言っても、まだ高い。',

  // ていう - casual pronunciation of という but used for naming
  '田中ていう人から電話があった。',

  // Simple quotational と (not the grammar pattern)
  '「行く」と言いました。',

  // からいうと - "from the standpoint of" / "in terms of" (different grammar)
  '品質からいうと、これがいい。',

  // でいうと - "expressed in terms of" / "in the context of" (different grammar)
  '日本語でいうと「愛」の意味です。',

  // について - "about" / "regarding" (different grammar)
  '日本について話しましょう。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
