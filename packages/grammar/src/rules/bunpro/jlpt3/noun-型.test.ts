import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './noun-型.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative tests - similar forms that should NOT match
const negatives = [
  // が (subject marker) + unrelated word
  '彼が持っている。',
  '私が来た。',

  // 形 (katachi) as standalone noun meaning "shape" (not suffix)
  '形が綺麗だ。',
  '形を見て。',

  // 型 as standalone verb (to mold/model) - not suffix usage
  '型を作る。',
  '型を取る。',

  // Different grammar with similar looking words
  '彼の方に行く。', // 方 (ほう - direction), not 型
  '読者の方へ。', // 方 (ほう - person), not 型
];

// GiNZA parsing limitation:
//
// このかたのスマホ - GiNZA parses "このかた" as a single adverb token
// instead of "この" (determiner) + "かた" (noun)
//
// Correct parse should be:
//   この (DET) + かた (NOUN) + の (ADP) + スマホ (NOUN)
//
// But GiNZA produces:
//   このかた (ADV, lemma=このかた) + の (ADP) + スマホ (NOUN)
//
// This is a GiNZA error - it merges "determiner + noun" into a single adverb token.
// We cannot match this pattern because:
// 1. "このかた" is a single token (can't match NOUN + の + かた pattern)
// 2. Matching single adverbs ending in "かた" would overcapture on:
//    - 別のかた (other person)
//    - どのかた (which person)
//    - そんなかた (that kind of person)
// These are legitimate adverbs meaning "person", not the noun-型 suffix.
//
// CONCLUSION: No reliable discriminator. GiNZA limitation.
const skipPositives = [
  'このかたのスマホはよく壊れます。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives, skipPositives });
});
