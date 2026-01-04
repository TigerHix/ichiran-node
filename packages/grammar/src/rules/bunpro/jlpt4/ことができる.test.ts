import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ことができる.js';
import { BUNPRO_JLPT4 } from './index.js';

const negatives = [
  // が without できる (different grammar)
  '私のことが好きだ。',
  'そのことが気になる。',
  // Simple できる without nominalization
  'できるならやってみて。',
  '時間があればできる。',
  // Noun + こと + が without できる (different grammar)
  '彼のことが心配だ。',
  'そんなことがありましたか。',
];

// GiNZA parsing limitation:
// 日本語を読むことができるだけで - GiNZA parses "できるだけ" as a single adverb token
// (pos=ADV, lemma=できるだけ) instead of "できる" (verb) + "だけ" (particle)
// This is a GiNZA tokenization issue where できる + だけ forms a compound adverb.
// Matching all ADV tokens with lemma=できるだけ would be too specific and wouldn't
// help us match the general pattern of verb + ことができる.
const skipPositives = [
  '日本語を読むことができるだけで、凄いと思います。',
];

describe('bunpro.jlpt4', () => {
  const engine = useSharedEngine([BUNPRO_JLPT4]);
  describeRule(rule, 'JLPT4', BUNPRO_JLPT4.id, engine.get, { negatives, skipPositives });
});
