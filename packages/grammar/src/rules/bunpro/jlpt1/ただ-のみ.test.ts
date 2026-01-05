import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ただ-のみ.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // だけ instead of のみ (informal, different pattern)
  'ただ待つだけだ。',
  'ただ祈るだけです。',
  '練習だけだ。',
  // ただ without のみ (not the complete pattern)
  'ただ待っている。',
  'ただ勉強する。',
  // Similar but different grammar (ただ～て form)
  'ただ待っているだけだ。',
  // Different meaning of ただ (free/ordinary) as ただの
  'ただの人が通った。',
  'これはただの水です。',
  // のみ with other expressions (not this grammar point)
  'それのみが重要だ。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
