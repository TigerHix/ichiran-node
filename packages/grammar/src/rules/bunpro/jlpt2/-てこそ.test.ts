import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './-てこそ.js';
import { BUNPRO_JLPT2 } from './index.js';

const negatives = [
  // て form followed by other particles (not こそ)
  '食べてから寝る。',
  '歩いても疲れない。',
  // こそ used for emphasis but not after て form
  '今日こそ頑張る。',
  '君こそが正しい。',
  // Separate clauses with て and こそ
  '本を読んで、今日こそ理解した。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});

