import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './そばから.js';
import { BUNPRO_JLPT1 } from './index.js';

const negatives = [
  // そば as location (side/beside) + から (from) - not the grammar pattern
  '家のそばから川が流れている。',
  '彼のそばから誰も離れない。',
  // そば as noun modifying location, not temporal connector
  'そばから風が吹いてくる。',
  // Verb + そば (without から) - different grammar
  '彼はいつも私のそばにいる。',
  // から as source/origin, not temporal connector
  'そばから来た人。',
];

describe('bunpro.jlpt1', () => {
  const engine = useSharedEngine([BUNPRO_JLPT1]);
  describeRule(rule, 'JLPT1', BUNPRO_JLPT1.id, engine.get, { negatives });
});
