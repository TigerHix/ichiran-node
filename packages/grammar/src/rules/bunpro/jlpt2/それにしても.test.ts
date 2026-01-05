import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './それにしても.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the それにしても grammar rule
const negatives = [
  // それに (soreni) - different conjunction meaning "and besides/moreover"
  // Example: 病気だ。それに、お金もない。
  '彼は頭がいい。それに、性格もいい。',
  '雨が降っている。それに、風も強い。',

  // それでも (soredemo) - "but still" (different grammar)
  '雨が降っている。それでも、行きます。',
  '失敗した。それでも、諦めません。',

  // それなのに (sorenanon) - "and yet" (stronger contrast)
  '勉強した。それなのに、試験に落ちた。',
  '早く出かけた。それなのに、遅刻した。',

  // にしても (nishitemo) - when used with other pronouns or nouns
  // Example: 私にしても、あなたにしても
  '私にしても同じだ。',
  '誰にしても難しいだろう。',

  // それ (sore) alone - demonstrative pronoun
  'それは私の本です。',
  'それを見せてください。',

  // しても (shitemo) alone - "even if (one) does"
  '何をしても無駄だ。',
  '誰が行っても同じです。',

  // Locative に + する (ni suru) - different grammar
  '英語にするか、日本語にするか。',

  // に + しても as separate components (not as conjunction)
  // Example: time/location + ni + suru
  '夜にしても昼にしても忙しい。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
