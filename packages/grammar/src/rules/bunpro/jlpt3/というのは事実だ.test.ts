import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './というのは事実だ.js';
import { BUNPRO_JLPT3 } from './index.js';

const negatives = [
  // ということだ (JLPT3) - "it means that / I hear that" (hearsay/meaning)
  // This is a DIFFERENT grammar point from というのは事実だ
  // The latter emphasizes objective FACT, while the former is hearsay/meaning
  '先生によると、この病気は薬では治せないということだ。',
  '人によって考え方が違うということだ。',

  // というのは (JLPT3) - nominalization "what is called ~"
  // This introduces a definition or explanation, not a fact
  'テレワークというのは、在宅で仕事をすることです。',

  // Simple nominalization + 事実だ (without proper という construction)
  // These should NOT match as they lack the grammatical structure
  '彼が来たこと事実だ。',

  // 事実 as a regular noun (not part of the grammar pattern)
  'これは事実です。',
  '事実を隠すな。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
