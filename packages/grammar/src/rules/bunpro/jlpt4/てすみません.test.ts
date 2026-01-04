import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てすみません.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match てすみません
const negatives = [
  // すみません as standalone apology (not preceded by te-form)
  'すみません、遅れました。',
  'すみませんが、道を教えてください。',

  // て form for other grammar (te-form + other auxiliary)
  '本を読んでいます。',  // ている (progressive)
  '窓を開けておきます。',  // ておく (do in advance)
  '先生が教えてくれた。',  // てくれる (someone does for me)
  '母を作ってあげた。',    // てあげる (do for someone)

  // Similar apology patterns but different grammar
  'ごめんなさい。',        // casual apology
  '申し訳ありません。',    // formal apology

  // すみません with different connectors
  '遅刻ですみません。',    // nominal + です + すみません
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives });
});
