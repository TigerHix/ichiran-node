import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことは-が.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  '大切なことを忘れた。',
  '漢字は読めますが、簡単な漢字しか読めないです。',
  '彼は来たが、彼女は来なかった。',
  '毎日運動することにした。',
  '田中さんのことだから、今日も遅れてくるだろう。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
