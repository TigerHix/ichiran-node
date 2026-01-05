import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './こそすれ-ない.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // Just こそ without the classical verb form
  'これはこそ大切だ。',
  '彼こそが犯人だ。',
  // こそ followed by regular verb forms (not classical)
  'こそするだろう。',
  'こそなっている。',
  // Similar patterns with different particles
  'これはましてありません。',
  'そればかりか、もっと悪い。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
