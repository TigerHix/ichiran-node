import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ところで.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // Similar conjunctions that should NOT match
  // ところが (however/on the contrary) - different conjunction
  '買ったところが、壊れていた。',
  '行ったところが、閉まっていた。',

  // それから (and then/after that)
  'それから、行きました。',

  // そこで (therefore/so) - different conjunction
  'そこで、警察を呼んだ。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
