import { describe } from 'bun:test';
import { useSharedEngine } from '../_test/engine.js';
import { describeRule } from '../_test/helpers.js';
import rule from './どうせ.js';
import { BUNPRO_JLPT2 } from './index.js';

// Negative test cases - sentences that should NOT match the どうせ grammar rule
const negatives = [
  // どう (dou) - "how" (interrogative adverb, different word)
  'どうやって駅に行きますか。',
  'どう思いますか。',
  'どうすればいいですか。',

  // Similar adverbs with different meanings
  '何しろ忙しいので行けません。',  // 何しろ (nanihiro) - explanatory
  '何といっても彼が一番だ。',      // 何といっても (nan to ittemo) - emphasizes quality
  'とにかく頑張りましょう。',        // とにかく (tonikaku) - "anyway" (less emotional)
  'やっぱり雨が降ってきた。',        // やっぱり (yappari) - "as expected"
  'やはり彼は来なかった。',          // やはり (yahari) - "as expected"
  '絶対に勝つ。',                    // 絶対に (zettai ni) - "absolutely"
  '必ず成功する。',                  // 必ず (kanarazu) - "certainly"
  'きっと雨が降るでしょう。',        // きっと (kitto) - "surely, probably"

  // Adverbs that might appear in similar contexts
  '結局行けなかった。',              // 結局 - "in the end"
  '結局同じことだ。',
  'やっぱりそうでしたか。',
  'やはり予想通りでした。',
];

describe('bunpro.jlpt2', () => {
  const engine = useSharedEngine([BUNPRO_JLPT2]);
  describeRule(rule, 'JLPT2', BUNPRO_JLPT2.id, engine.get, { negatives });
});
