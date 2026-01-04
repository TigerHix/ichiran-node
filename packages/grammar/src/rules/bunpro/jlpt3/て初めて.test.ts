import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './て初めて.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Simple て-form without 初めて (te-form connector)
  '朝ごはんを食べて学校に行きました。',
  '本を読んで寝ました。',

  // てから (after doing - different grammar)
  '日本語を勉強してから、日本に来ました。',
  '食べてから、勉強します。',

  // 初めて alone (adverb "for the first time")
  '日本に来たのは初めてです。',
  '初めて会った時から好きでした。',

  // Verb + て + other adverbs (not 初めて)
  '勉強してよく分かりました。',
  '行ってみました。',

  // Noun + で + 初めて (different structure)
  // This would typically be "Xで初めてY" as in "by means of X, for the first time Y"
  // which is a different grammatical construction
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
