import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './てよかった.js';
import { BUNPRO_JLPT4 } from './index.js';

// Negative examples: sentences that should NOT match てよかった
const negatives = [
  // Simple よかった without te-form (different grammar)
  'よかった。',
  '結果はよかった。',
  // ばよかった (regret - different grammar point)
  '行けばよかった。',
  'すればよかったです。',
  // て form followed by unrelated よかった (not the grammar pattern)
  '買って、よかったと思った。',
  // よい adjective in other contexts
  'それはよい本です。',
  '今日はよい天気ですね。',
];

// Sentences that can't be matched due to GiNZA parsing limitations:
//
// ANALYSIS: Potential verb in te-form (会えてよかった)
//
// GiNZA parses "会えて" as a single VERB token (lemma=会う, inflectionForm=連用形-一般)
// Unlike standard te-forms which are verb + て(SCONJ), potential verb te-forms are
// tokenized as single tokens without a separate て particle.
//
// The issue is that we need to distinguish:
// - 会えてよかった (potential verb te-form + よかった - SHOULD match)
// - よかった (standalone "was good" - should NOT match)
//
// Without a separate て/で particle to detect, we would need to:
// 1. Match any VERB/AUX before よかった - but this would match standalone よかった
// 2. Use inflectionForm to detect te-forms - but potential verbs use 連用形-一般,
//    same as other conjugations
//
// Using looser constraints (just matching any token before よかった) would cause
// overcapture on unrelated patterns like "買って、よかったと思った" where よかった
// is not attached to the te-form verb.
//
// CONCLUSION: No reliable discriminator without separate て particle. GiNZA limitation.
const skipPositives = [
  'おばあちゃんが亡くなる前にもう一度あえてよかった。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
