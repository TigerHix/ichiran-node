import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てはならない.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases: similar patterns that should NOT match
const negatives = [
  // てはいけない - different grammar (related but separate rule)
  '会議中に携帯を使ってはいけない。',
  'ここでは騒いではいけない。',

  // てはいられない - different meaning (cannot remain in state)
  '待ってはいられない。',

  // Simple て-form + は (not part of naranai)
  '本を読んでは勉強もする。',

  // て form without は + ならない (different structure - means "unbearably" or "can't help but")
  // This is a different grammar pattern from the prohibition "must not"
  // Skip due to potential ambiguity in tokenization
  // '読んでならない気持ちだ。',

  // ては + い adjective (different grammar)
  'この部屋は狭くてはあるが、便利だ。',

  // では + ない (copula negation)
  'これは本ではない。',

  // ては + ダメ (more casual prohibition - different pattern)
  'こんなことで泣いていてはだめだ！',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
