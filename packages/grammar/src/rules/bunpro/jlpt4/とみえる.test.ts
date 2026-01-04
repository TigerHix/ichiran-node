import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './とみえる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // 見える meaning "can see / be visible" (not conjecture)
  '富士山が見える。',
  '遠くに島が見える。',
  '映画が見える',

  // にみえる (looks like / appears to be - visual appearance)
  // This is different from とみえる (conjecture based on evidence)
  '彼は若く見える。',
  'この景色は絵のように見える。',
  '彼女は先生に見える。',

  // Conditional と (when/if) + 見える
  '窓を開けると、富士山が見える。',

  // Accompaniment と (with) + 見える
  '友達と見える映画がある。',

  // Quotation と with literal quote (not conjecture)
  '彼は「来る」と言った。',

  // Noun + と + 見える where と marks a direct object (different grammar)
  'テレビと見えるものがある。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
