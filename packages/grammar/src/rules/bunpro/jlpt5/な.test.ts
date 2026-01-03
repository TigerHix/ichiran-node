import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './な.js';
import { BUNPRO_JLPT5 } from './index.js';

// Negative test cases - sentences that should NOT match the prohibitive な rule
const negatives = [
  // Na-adjective + な + noun (different な - it's pos=AUX, lemma=だ, not pos=PART, lemma=な)
  '静かな夜です。',
  '元気な子供。',
  'きれいな花。',
  '大好きな食べ物。',
  '彼は有名な人です。',
  // な as part of ない (negation auxiliary)
  '行かなきゃ。',
  '分かりません。',
  '食べないでください。',
  // な inside words (not the prohibitive particle)
  '七曜日。',
  '父なしで生きる。',
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
