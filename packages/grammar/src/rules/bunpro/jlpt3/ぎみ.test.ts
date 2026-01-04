import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './ぎみ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: Similar patterns that should NOT match
const negatives = [
  // げ ( JLPT2 suffix for perceived emotions, e.g. 寂しげ, 悲しげ)
  '彼女は寂しげな顔をしている。',
  '悲しげな音楽が流れている。',
  // っぽい (JLPT3 suffix for characteristic, e.g. 子どもっぽい, 忘れっぽい)
  '彼は子どもっぽい性格だ。',
  '最近忘れっぽい。',
  // がち (JLPT3 suffix for tendency, e.g. 病気がち, 遅れがち)
  '彼女は病気がちだ。',
  'この電車は遅れがちだ。',
  // ような気がする (different grammar: "have a feeling that")
  '雨が降るような気がする。',
  // 気味 as independent noun (sensation/feeling)
  '悪い気味がする。',
  '変な気味だ。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
