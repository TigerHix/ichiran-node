import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './さ-filler.js';
import { BUNPRO_JLPT3 } from './index.js';

// Negative examples: sentences that should NOT match さ-filler
const negatives = [
  // さ as interjection at sentence start (さ-interjection grammar point)
  'さ、始めましょう。',
  'さあ、どうぞ。',
  'さぁ、食べてください。',

  // Similar particles (ね, よ) - should not match this rule
  'それね、面白いよ。',
  'ね、聞いてよ。',
  'あのね、実はね。',

  // さえ (even) - completely different grammar
  '子供さえわかる。',
  '雨さえ降れば。',

  // さて (well/then) - conjunction
  'さて、次に行きましょう。',
  'さて、どうしましょう。',

  // そう (so) - different word
  'そうですね。',
  'そう、そうです。',

  // Note: さ-casualよ (sentence-final) and さ-filler are ambiguous in GiNZA
  // Both parse as: text=さ, pos=PART, tag=助詞-終助詞, dep=mark
  // The only difference is whether さ is followed by punctuation。
  // Due to GiNZA limitations, we cannot reliably distinguish these cases.
  // Sentences like "それはいいさ。" could match either rule depending on context.
];

describe('bunpro.jlpt3', () => {
  const engine = useSharedEngine([BUNPRO_JLPT3]);
  describeRule(rule, 'JLPT3', BUNPRO_JLPT3.id, engine.get, { negatives });
});
