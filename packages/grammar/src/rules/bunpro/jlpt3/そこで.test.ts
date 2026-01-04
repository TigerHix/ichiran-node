import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そこで.js';
import { BUNPRO_JLPT3 } from './index.js';

// Note: This rule matches both conjunction use (therefore/so) and locative use (at that place)
// since GiNZA doesn't consistently distinguish them with dep labels.
// This is similar to それに which also matches both uses.

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get);
});
