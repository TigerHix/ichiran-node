import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './かねない.js';
import { BUNPRO_JLPT2 } from './index.js';

// False positives: sentences that should NOT match
const negatives = [
  // かねる (positive form - different grammar: "cannot do/hesitant to do")
  'その件については答えかねます。',
  '個人情報はお教えしかねます。',
  '判断しかねます。',
  'そうとは言いがねる。',
  '承知しかねます。',

  // Other potential negative patterns
  // ～がたい (difficult to do - different grammar)
  '得がたい結果だ。',
  '理解しがたい。',

  // ～にくい (hard to do - different grammar)
  '食べにくい。',
  '使いにくい道具。',

  // ～づらい (painful to do - different grammar)
  '言いづらい。',
  '歩きづらい。',

  // Verb stem + ない (simple negation, not かねない)
  '食べない。',
  '行かない。',
  'しない。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
