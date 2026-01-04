import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './たらいい-といい.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ばいい (JLPT3) - different grammar point with lower certainty
  // "it would be good if you did" (advice)
  '届ければいいんだけど。',

  // たほうがいい (JLPT5) - suggestion/advice, not wish
  // "you should do X" vs "it would be good if X happened"
  'したほうがいい。',
  '行ったほうがいい。',

  // ていい - "it's okay to do X" (permission)
  'してもいい。',
  '言ってもいい。',

  // ただしいい - "good if correct" (different meaning)
  // This would be an adjective ただしい + いい, not our grammar
  // (unlikely sentence but shows the pattern difference)

  // Adjective + い + いい - not our pattern
  // When い is part of adjective stem, not conditional marker
  // (this is hard to construct as natural sentence)

  // Noun + といい - different grammar (quotational + good)
  // "田中さんといい" = "Tanaka-san is good" vs our pattern
  // This is actually a different structure (noun + to + ii)
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
