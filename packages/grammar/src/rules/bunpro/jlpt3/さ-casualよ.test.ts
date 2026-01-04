import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さ-casualよ.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences that should NOT match さ-casualよ
const negatives = [
  // Similar particles (よ, ね) - should not match this rule
  'そんなことはないよ。',
  'そんなことないね。',
  '知らないよ。',
  '行こうね。',
  // Polite forms (さ is casual only, not for polite speech)
  'そうですね。',
  'わかりましたね。',
  // さ as mid-sentence filler (different grammar point: さ-filler)
  // The filler usage appears mid-sentence, not at the end
  'さ、行きましょう。',
  'さあ、どうしましょう。',
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
