import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './けっこう.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative examples: けっこう should NOT match in these cases
const negatives = [
  // Other similar adverbs that aren't けっこう
  'これはとても高いです。',           // とても (very) - different adverb
  'これはかなり高いです。',           // かなり (considerably) - different adverb
  'これはすごく高いです。',           // すごく (extremely) - different adverb
  'これはちょっと高いです。',          // ちょっと (a little) - different adverb
  // Different grammatical uses
  '結婚式はとても立派だった。',       // 結婚式 (wedding ceremony) - different word with 結 reading
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
