import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './あれ.js';
import { BUNPRO_JLPT5 } from './index.js';

const negatives = [
  // これ (this - near speaker)
  'これは本です。',
  // それ (that - near listener)
  'それはペンです。',
  // Aré as interjection (different word - "huh?" / "oh?")
  // This would be parsed differently (rising intonation, different context)
  'あれ？何か落ちましたよ。', // "Huh? Something fell."
];

describe('bunpro.jlpt5', () => {
  const engine = useSharedEngine([BUNPRO_JLPT5]);
  describeRule(rule, 'JLPT5', BUNPRO_JLPT5.id, engine.get, { negatives });
});
